open! Core0

let gray_inc_mux_inputs (type a) (module Comb : Comb.S with type t = a) width ~by : a list
  =
  List.init (1 lsl width) ~f:(fun i ->
    Comb.of_unsigned_int ~width i |> Comb.gray_increment ~by)
;;

module type S = sig
  val width : int
  val log2_depth : int
  val optimize_for_same_clock_rate_and_always_reading : bool
end

module Make (M : S) = struct
  open Clocked_design
  open Signal

  let address_width = M.log2_depth
  let fifo_capacity = 1 lsl address_width
  let gray_inc ~by x = mux x (gray_inc_mux_inputs (module Signal) address_width ~by)
  let read_dom = Clock_domain.create "read"
  let write_dom = Clock_domain.create "write"

  module I = struct
    type 'a t =
      { clock_write : 'a
      ; clock_read : 'a
      ; reset_write : 'a
      ; reset_read : 'a
      ; data_in : 'a [@bits M.width]
      ; write_enable : 'a
      ; read_enable : 'a
      }
    [@@deriving hardcaml]

    let domains =
      { clock_write = write_dom
      ; reset_write = write_dom
      ; write_enable = write_dom
      ; data_in = write_dom
      ; clock_read = read_dom
      ; reset_read = read_dom
      ; read_enable = read_dom
      }
    ;;
  end

  module O = struct
    type 'a t =
      { full : 'a
      ; data_out : 'a [@bits M.width]
      ; valid : 'a
      ; almost_empty : 'a
      }
    [@@deriving hardcaml]

    let domains =
      { full = write_dom; data_out = read_dom; valid = read_dom; almost_empty = read_dom }
    ;;
  end

  module Async_distributed_ram = struct
    type t =
      { clock_write : Signal.t
      ; clock_read : Signal.t
      ; multiport_mem : Signal.t
      ; mutable is_written : bool
      ; mutable is_read : bool
      ; write_enable : Always.Variable.t
      ; write_address : Signal.t
      ; write_data : Signal.t
      ; read_address : Signal.t
      }

    let create_clocked ~name ~clock_write ~clock_read =
      let read_dom = Signal.get_domain clock_read in
      let write_dom = Signal.get_domain clock_write in
      let write_data = Signal.wire M.width ~dom:write_dom in
      let write_address = Signal.wire address_width ~dom:write_dom in
      let read_address = Signal.wire address_width ~dom:read_dom in
      let write_enable = Always.Variable.wire ~dom:write_dom ~default:Signal.gnd in
      let multiport_mem =
        Signal.multiport_memory
          ~name
          ~attributes:[ Rtl_attribute.Vivado.Ram_style.distributed ]
          ~write_ports:
            [| { write_clock = clock_write
               ; write_data
               ; write_enable = write_enable.value
               ; write_address
               }
            |]
          ~read_addresses:[| read_address |]
          fifo_capacity
      in
      let multiport_mem = multiport_mem.(0) in
      { clock_write
      ; clock_read
      ; write_address
      ; write_enable
      ; write_data
      ; read_address
      ; multiport_mem
      ; is_written = false
      ; is_read = false
      }
    ;;

    let read t ~address =
      let read_dom = Signal.get_domain t.clock_read in
      assert (width address = address_width);
      if t.is_read then raise_s [%message "Async_ram has already previously been read"];
      t.is_read <- true;
      t.read_address <-- address;
      t.multiport_mem |> Clocked_signal.Unsafe.set_domain ~dom:read_dom
    ;;

    let write t ~address ~data =
      assert (width address = address_width);
      assert (width data = M.width);
      if t.is_written
      then raise_s [%message "Async_ram has already previously been written"];
      t.is_written <- true;
      t.write_address <-- address;
      t.write_data <-- data;
      Always.(t.write_enable <--. 1)
    ;;
  end

  type raddr_wd =
    { raddr_wd : Always.Variable.t
    ; raddr_wd_ffs : Always.Variable.t array
    }

  let create_internal_clocked
    ?(use_synchronous_clear_semantics = false)
    ?(use_negedge_sync_chain = false)
    ?(sync_stages = 2)
    ?scope
    (i : _ I.t)
    =
    if sync_stages < 2
    then raise_s [%message "[sync_stages] must be >= 2!" (sync_stages : int)];
    if use_negedge_sync_chain && sync_stages % 2 = 1
    then
      raise_s
        [%message "[sync_stages] must be even when using negedge!" (sync_stages : int)];
    let ( -- ) =
      match scope with
      | Some scope -> Scope.naming_clocked scope
      | None -> ( -- )
    in
    let reg_spec ~clock ~reset =
      if use_synchronous_clear_semantics
      then Reg_spec.create ~clock ~clear:reset ()
      else Reg_spec.create ~clock ~reset ()
    in
    let async_reg_var ?clock_edge () ~name ~clock ~reset ~width =
      let spec = reg_spec ~clock ~reset |> Reg_spec.override ?clock_edge in
      let var = Always.Variable.reg spec ~enable:vdd ~width in
      ignore
        (Signal.add_attribute var.value (Rtl_attribute.Vivado.async_reg true) -- name
         : Signal.t);
      var
    in
    let reg_var ~name ~clock ~reset ~width =
      let var = Always.Variable.reg (reg_spec ~clock ~reset) ~enable:vdd ~width in
      ignore
        (Signal.add_attribute var.value (Rtl_attribute.Vivado.dont_touch true) -- name
         : Signal.t);
      var
    in
    let waddr_rd =
      async_reg_var
        ()
        ~clock:i.clock_read
        ~reset:i.reset_read
        ~width:address_width
        ~name:"waddr_rd"
    in
    let clock_edge_of_sync_chain idx =
      if use_negedge_sync_chain
      then if idx % 2 = 0 then Edge.Falling else Rising
      else Rising
    in
    let waddr_rd_ffs =
      Array.init (sync_stages - 1) ~f:(fun idx ->
        async_reg_var
          ~clock_edge:(clock_edge_of_sync_chain idx)
          ()
          ~name:[%string "waddr_rd_ff_%{idx#Int}"]
          ~clock:i.clock_read
          ~reset:i.reset_read
          ~width:address_width)
    in
    let raddr_rd =
      reg_var
        ~clock:i.clock_read
        ~reset:i.reset_read
        ~width:address_width
        ~name:"raddr_rd"
    in
    let data_out =
      async_reg_var
        ()
        ~clock:i.clock_read
        ~reset:i.reset_read
        ~width:M.width
        ~name:"data_out"
    in
    let waddr_wd =
      reg_var
        ~clock:i.clock_write
        ~reset:i.reset_write
        ~width:address_width
        ~name:"waddr_wd"
    in
    let raddr_wd =
      if M.optimize_for_same_clock_rate_and_always_reading
      then None
      else (
        let raddr_wd =
          async_reg_var
            ()
            ~clock:i.clock_write
            ~reset:i.reset_write
            ~width:address_width
            ~name:"raddr_wd"
        in
        let raddr_wd_ffs =
          Array.init (sync_stages - 1) ~f:(fun idx ->
            async_reg_var
              ~clock_edge:(clock_edge_of_sync_chain idx)
              ()
              ~name:[%string "raddr_wd_ff_%{idx#Int}"]
              ~clock:i.clock_write
              ~reset:i.reset_write
              ~width:address_width)
        in
        Some { raddr_wd; raddr_wd_ffs })
    in
    let full =
      match raddr_wd with
      | None -> gnd
      | Some { raddr_wd; _ } -> gray_inc ~by:1 waddr_wd.value ==: raddr_wd.value
    in
    let vld = waddr_rd.value <>: raddr_rd.value in
    let almost_empty =
      let current = waddr_rd.value ==: raddr_rd.value in
      let one_ahead = waddr_rd.value ==: gray_inc ~by:1 raddr_rd.value in
      let two_ahead_if_possible =
        if address_width = 1
        then gnd (* fifo not large enough to look this far ahead *)
        else waddr_rd.value ==: gray_inc ~by:2 raddr_rd.value
      in
      current |: one_ahead |: two_ahead_if_possible
    in
    let ram =
      Async_distributed_ram.create_clocked
        ~name:(Option.value_map scope ~default:"ram" ~f:(fun s -> Scope.name s "ram"))
        ~clock_write:i.clock_write
        ~clock_read:i.clock_read
    in
    let raddr_rd_next =
      let read_enable =
        if M.optimize_for_same_clock_rate_and_always_reading then vdd else i.read_enable
      in
      mux2 (read_enable &: vld) (gray_inc ~by:1 raddr_rd.value) raddr_rd.value
    in
    let read_dom = Signal.get_domain i.clock_read in
    let write_dom = Signal.get_domain i.clock_write in
    Always.(
      compile
        [ (* @(posedge clk_read) *)
          Array.init (Array.length waddr_rd_ffs) ~f:(fun idx ->
            if idx = 0
            then
              waddr_rd_ffs.(idx)
              <-- Clocked_signal.Unsafe.set_domain ~dom:read_dom waddr_wd.value
            else waddr_rd_ffs.(idx) <-- waddr_rd_ffs.(idx - 1).value)
          |> Array.to_list
          |> proc
        ; waddr_rd <-- (Array.last_exn waddr_rd_ffs).value
        ; (* @(posedge clk_write) *)
          (match raddr_wd with
           | Some { raddr_wd_ffs; raddr_wd } ->
             proc
               [ Array.init (Array.length raddr_wd_ffs) ~f:(fun idx ->
                   if idx = 0
                   then
                     raddr_wd_ffs.(idx)
                     <-- Clocked_signal.Unsafe.set_domain ~dom:write_dom raddr_rd.value
                   else raddr_wd_ffs.(idx) <-- raddr_wd_ffs.(idx - 1).value)
                 |> Array.to_list
                 |> proc
               ; raddr_wd <-- (Array.last_exn raddr_wd_ffs).value
               ]
           | None -> proc [])
        ; (* @(posedge clk_write) *)
          when_
            (i.write_enable &: ~:full)
            [ Async_distributed_ram.write ram ~address:waddr_wd.value ~data:i.data_in
            ; waddr_wd <-- gray_inc ~by:1 waddr_wd.value
            ]
        ; (* @(posedge clk_read) *)
          data_out <-- Async_distributed_ram.read ram ~address:raddr_rd_next
        ; (* @(posedge clk_read) *)
          raddr_rd <-- raddr_rd_next
        ]);
    { O.full; data_out = data_out.value; valid = vld; almost_empty }
  ;;

  let create_clocked = create_internal_clocked ~use_synchronous_clear_semantics:false

  let create_with_delay_clocked ?(delay = 0) scope (i : _ I.t) =
    let read_dom = Signal.get_domain i.clock_read in
    let ( -- ) = Scope.naming_clocked scope in
    let async_fifo_has_valid_value = wire ~dom:read_dom 1 in
    (* If delay is 0 we don't count, otherwise start a counter each time we see valid low
       on the FIFO output, and only set it back to 0 when the FIFO valid goes low again *)
    let delay_val =
      if delay = 0
      then vdd
      else (
        let min_bits = Bits.num_bits_to_represent delay in
        let spec = Reg_spec.create () ~clock:i.clock_read ~reset:i.reset_read in
        let delay_cnt_wire = wire ~dom:read_dom min_bits in
        let delay_cnt =
          reg_fb spec ~enable:vdd ~width:min_bits ~f:(fun d ->
            mux2
              async_fifo_has_valid_value
              (mux2 (delay_cnt_wire ==:. delay) d (d +:. 1))
              (zero min_bits))
        in
        delay_cnt_wire <-- delay_cnt;
        delay_cnt ==:. delay)
    in
    let async_fifo =
      create_clocked
        ~scope
        { i with
          read_enable = (i.read_enable &: delay_val) -- "read_en"
        ; write_enable = i.write_enable -- "write_en"
        ; data_in = i.data_in -- "data_in"
        }
    in
    async_fifo_has_valid_value <-- async_fifo.valid;
    { O.full = async_fifo.full -- "full"
    ; data_out = async_fifo.data_out
    ; valid = async_fifo.valid &: delay_val
    ; almost_empty = async_fifo.almost_empty
    }
  ;;

  module H = Hierarchy.In_clocked_scope (I) (O)

  let make_create_or_hierarchical_basic
    ~how_to_instantiate
    ~use_synchronous_clear_semantics
    ?name
    ?use_negedge_sync_chain
    ?sync_stages
    ?scope
    input
    =
    let scope =
      match scope with
      | None -> Scope.create ()
      | Some scope -> scope
    in
    H.hierarchical
      ?name
      ~how_to_instantiate
      ~caller_signal_type:Signal
      ~scope
      (fun scope (input : _ I.t) ->
        create_internal_clocked
          ~use_synchronous_clear_semantics
          ?use_negedge_sync_chain
          ?sync_stages
          ~scope
          input)
      input
  ;;

  let make_create_or_hierarchical_delayed ?name ~how_to_instantiate ~delay ~scope input =
    H.hierarchical
      ?name
      ~how_to_instantiate
      ~caller_signal_type:Signal
      ~scope
      (create_with_delay_clocked ?delay)
      input
  ;;

  let create_with_delay ?delay scope (i : _ I.t) =
    make_create_or_hierarchical_delayed ~delay ~how_to_instantiate:Inlined ~scope i
  ;;

  (* The tcl scripts that constrain the name of this module depend on the module name
     being [hardcaml_async_fifo*]. *)
  let base_name = "hardcaml_async_fifo"

  let create =
    make_create_or_hierarchical_basic
      ~name:base_name (* We're inlining, the exact name here doesn't really matter *)
      ~use_synchronous_clear_semantics:false
      ~how_to_instantiate:Inlined
  ;;

  let hierarchical ?(name = base_name) ?use_negedge_sync_chain ?sync_stages scope i =
    make_create_or_hierarchical_basic
      ~how_to_instantiate:Hierarchical_or_inlined_by_scope
      ~use_synchronous_clear_semantics:false
      ~name
      ?use_negedge_sync_chain
      ?sync_stages
      ~scope
      i
  ;;

  let hierarchical_with_delay ?(name = [%string "%{base_name}_with_delay"]) ?delay scope i
    =
    make_create_or_hierarchical_delayed
      ~name
      ~how_to_instantiate:Inlined_in_scope
      ~delay
      ~scope
      i
  ;;

  module For_testing = struct
    let create_with_synchronous_clear_semantics_for_simulation_only
      ?use_negedge_sync_chain
      ?sync_stages
      ?scope
      input
      : _ O.t
      =
      make_create_or_hierarchical_basic
        ~how_to_instantiate:Inlined
        ~use_synchronous_clear_semantics:true
        ?use_negedge_sync_chain
        ?sync_stages
        ?scope
        input
    ;;
  end
end

module For_testing = struct
  let gray_inc_mux_inputs = gray_inc_mux_inputs
end
