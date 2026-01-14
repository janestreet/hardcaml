open! Core0
open Signal
include Fifo_intf.T
module Kinded_fifo = Fifo_intf.Kinded_fifo

let naming ?scope =
  match scope with
  | Some scope -> Scope.naming scope
  | None -> ( -- )
;;

(* Generates wbr memory with explicit collision detection to gurantee [wbr] behaviour.
   Despite what's suggested by Vivado's BRAM documentation, [write_first] are not
   respected, even in SDP RAM mode.
*)
let ram_wbr_safe
  scope
  capacity
  ~(write_port : _ Write_port.t)
  ~(read_port : _ Read_port.t)
  ~ram_attributes
  =
  let open Signal in
  let ( -- ) = naming ?scope in
  (* We don't need collision detection when using distributed RAM *)
  if List.find
       ram_attributes
       ~f:(Rtl_attribute.equal Rtl_attribute.Vivado.Ram_style.distributed)
     |> Option.is_some
  then
    ram_wbr capacity ~attributes:ram_attributes ~write_port ~read_port -- "ram_rbw_data"
  else (
    let collision =
      reg
        (Reg_spec.create ~clock:write_port.write_clock ())
        ~enable:read_port.read_enable
        (write_port.write_enable
         &: read_port.read_enable
         &: (write_port.write_address ==: read_port.read_address))
      -- "collision"
    in
    let data_before_collision =
      reg
        (Reg_spec.create ~clock:write_port.write_clock ())
        ~enable:read_port.read_enable
        write_port.write_data
      -- "data_before_collision"
    in
    mux2
      collision
      data_before_collision
      (ram_rbw capacity ~attributes:ram_attributes ~write_port ~read_port
       -- "ram_rbw_data"))
;;

let capacity_and_used_bits showahead ram_capacity =
  (* to be consistent with Vivado's FIFO implementation, when instantiating a fwft FIFO,
     it's actual capacity is one more due to the additional register in the prefetch
     buffer register. *)
  let actual_capacity = if showahead then ram_capacity + 1 else ram_capacity in
  let used_bits = num_bits_to_represent actual_capacity in
  actual_capacity, used_bits
;;

(* This module implements code that can be shared between different fifo implementations
   for tracking capacity based on write and read signals. *)
module Fifo_helper (Config : sig
    val used_bits : int
    val actual_capacity : int
  end) =
struct
  open Config

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; wr : 'a
      ; rd : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { used : 'a [@bits used_bits]
      ; used_next : 'a [@bits used_bits]
      ; empty : 'a
      ; nearly_empty : 'a
      ; full : 'a
      ; nearly_full : 'a
      }
    [@@deriving hardcaml]
  end

  let create ?nearly_full ?(nearly_empty = 1) ?scope ({ clock; clear; wr; rd } : _ I.t)
    : _ O.t
    =
    let ( -- ) = naming ?scope in
    let reg_spec = Reg_spec.create ~clock ~clear () in
    let reg ?initialize_to ~enable d =
      reg
        reg_spec
        ?clear_to:(Option.map initialize_to ~f:Signal.of_bits)
        ?initialize_to
        ~enable
        d
    in
    (* get nearly full/empty levels *)
    let nearly_full =
      match nearly_full with
      | None -> actual_capacity - 1
      | Some x -> x
    in
    let not_empty, full = wire 1, wire 1 in
    let empty = ~:not_empty in
    (* read or write, but not both *)
    let enable = rd ^: wr in
    (* fill level of fifo *)
    let used = wire used_bits in
    let used_plus_1 = (* for retiming *) wire used_bits in
    let used_minus_1 = wire used_bits in
    let used_next =
      mux2 enable (mux2 rd used_minus_1 used_plus_1) used
      (* read+write, or none *)
    in
    used <-- reg ~enable (used_next -- "USED_NEXT") -- "USED";
    used_plus_1
    <-- reg ~enable ~initialize_to:(Bits.one (width used)) (used_next +:. 1)
        -- "USED_PLUS_1";
    used_minus_1
    <-- reg ~enable ~initialize_to:(Bits.ones (width used)) (used_next -:. 1)
        -- "USED_MINUS_1";
    (* full empty flags *)
    not_empty <-- reg ~enable (used_next <>:. 0) -- "not_empty";
    full <-- reg ~enable (used_next ==:. actual_capacity) -- "full";
    (* nearly full/empty flags *)
    let nearly_empty =
      reg ~enable ~initialize_to:Bits.vdd (used_next <=:. nearly_empty) -- "nearly_empty"
    in
    let nearly_full = reg ~enable (used_next >=:. nearly_full) -- "nearly_full" in
    { used; used_next; empty; nearly_empty; full; nearly_full }
  ;;
end

let create
  ?read_latency
  ?(showahead = false)
  ?nearly_empty
  ?nearly_full
  ?(overflow_check = true)
  ?(underflow_check = true)
  ?(ram_attributes = [ Rtl_attribute.Vivado.Ram_style.block ])
  ?scope
  ()
  ~capacity:ram_capacity
  ~clock
  ~clear
  ~wr
  ~d
  ~rd
  =
  let ( -- ) = naming ?scope in
  (* Check if read_latency is set that its value makes sense. *)
  Option.iter read_latency ~f:(fun read_latency ->
    if showahead && read_latency <> 0
    then
      raise_s
        [%message
          "Cannot set showahead = true and read_latency <> 0 for Fifo."
            (read_latency : int)
            (showahead : bool)]);
  let reg_spec = Reg_spec.create ~clock ~clear () in
  let reg ?initialize_to ~enable d =
    reg
      reg_spec
      ?clear_to:(Option.map initialize_to ~f:Signal.of_bits)
      ?initialize_to
      ~enable
      d
  in
  let full', empty' = wire 1, wire 1 in
  let rd = if underflow_check then (rd &: ~:empty') -- "RD_INT" else rd in
  let wr = if overflow_check then (wr &: ~:full') -- "WR_INT" else wr in
  let abits = address_bits_for ram_capacity in
  let actual_capacity, used_bits = capacity_and_used_bits showahead ram_capacity in
  let module Fifo_helper =
    Fifo_helper (struct
      let used_bits = used_bits
      let actual_capacity = actual_capacity
    end)
  in
  let%tydi { used; used_next; empty; nearly_empty; full; nearly_full } =
    Fifo_helper.create ?nearly_full ?nearly_empty ?scope { clock; clear; wr; rd }
  in
  full' <-- full;
  empty' <-- empty;
  (* read/write addresses within fifo *)
  let addr_count enable name =
    let a = wire abits in
    let an = mod_counter ~max:(ram_capacity - 1) a in
    a
    <-- add_attribute (reg ~enable an -- name) (Rtl_attribute.Vivado.extract_reset false);
    a, an -- (name ^ "_NEXT")
  in
  let q =
    if showahead
    then (
      let used_is_one = reg ~enable:(rd ^: wr) (used_next ==:. 1) -- "used_is_one" in
      let used_gt_one = reg ~enable:(rd ^: wr) (used_next >:. 1) -- "used_gt_one" in
      let memory =
        let wr = wr &: (used_gt_one |: (used_is_one &: ~:rd)) in
        let rd = rd &: used_gt_one in
        let ra, ra_n = addr_count rd "READ_ADDRESS" in
        let ra = mux2 rd ra_n ra -- "RA" in
        let wa, _ = addr_count wr "WRITE_ADDRESS" in
        ram_wbr_safe
          scope
          ~ram_attributes
          ram_capacity
          ~write_port:
            { write_clock = clock; write_enable = wr; write_address = wa; write_data = d }
          ~read_port:{ read_clock = clock; read_enable = vdd; read_address = ra }
        -- "memory"
      in
      let bypass_cond = (empty &: wr |: (used_is_one &: wr &: rd)) -- "bypass_cond" in
      mux2 bypass_cond d memory |> reg ~enable:(bypass_cond |: rd))
    else (
      let ra, _ = addr_count rd "READ_ADDRESS" in
      let wa, _ = addr_count wr "WRITE_ADDRESS" in
      let spec = Reg_spec.create ~clock ~clear () in
      ram_rbw
        ~attributes:ram_attributes
        ram_capacity
        ~write_port:
          { write_clock = clock; write_enable = wr; write_address = wa; write_data = d }
        ~read_port:{ read_clock = clock; read_enable = rd; read_address = ra }
      |> pipeline spec ~n:(Option.value read_latency ~default:1 - 1))
  in
  { q
  ; full
  ; empty
  ; nearly_full
  ; nearly_empty
  ; used
  ; rd_rst_busy = gnd
  ; wr_rst_busy = gnd
  }
;;

let create_classic_with_extra_reg
  ?nearly_empty
  ?nearly_full
  ?overflow_check
  ?underflow_check
  ?ram_attributes
  ?scope
  ()
  ~capacity
  ~clock
  ~clear
  ~wr
  ~d
  ~rd
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let fifo_valid = wire 1 in
  let middle_valid = wire 1 in
  let fifo_rd_en = wire 1 in
  let empty = ~:(fifo_valid |: middle_valid) in
  let will_update_dout = ~:empty &: rd in
  let will_update_middle = fifo_valid &: (middle_valid ==: will_update_dout) in
  let fifo =
    create
      ~showahead:false
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd:fifo_rd_en
  in
  let middle_dout = reg spec ~enable:will_update_middle fifo.q in
  fifo_rd_en <-- (~:(fifo.empty) &: ~:(middle_valid &: fifo_valid));
  fifo_valid
  <-- reg spec ~enable:(fifo_rd_en |: will_update_middle |: will_update_dout) fifo_rd_en;
  middle_valid
  <-- reg spec ~enable:(will_update_middle |: will_update_dout) will_update_middle;
  { fifo with
    q = reg spec ~enable:will_update_dout (mux2 middle_valid middle_dout fifo.q)
  ; empty
  }
;;

let showahead_fifo_of_classic_fifo
  (create_fifo :
    capacity:int
    -> write_clock:Signal.t
    -> read_clock:Signal.t
    -> clear:Signal.t
    -> wr:Signal.t
    -> d:Signal.t
    -> rd:Signal.t
    -> (Signal.t, [ `Classic ]) Kinded_fifo.t)
  =
  Staged.stage (fun ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd ->
    let spec = Reg_spec.create ~clock:read_clock ~clear () in
    let fifo_rd_en = wire 1 in
    let (Classic fifo) =
      create_fifo ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd:fifo_rd_en
    in
    let dout_valid = reg spec ~enable:(fifo_rd_en |: rd) fifo_rd_en in
    let empty = ~:dout_valid in
    fifo_rd_en <-- (~:(fifo.empty) &: (~:dout_valid |: rd));
    Kinded_fifo.Showahead { fifo with empty })
;;

let create_showahead_from_classic
  ?nearly_empty
  ?nearly_full
  ?overflow_check
  ?underflow_check
  ?ram_attributes
  ?scope
  ()
  =
  let create_fifo ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd =
    assert (Signal.equal write_clock read_clock);
    create
      ~showahead:false
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity
      ~clock:write_clock
      ~clear
      ~wr
      ~d
      ~rd
    |> Kinded_fifo.Classic
  in
  let create_showahead_fifo =
    Staged.unstage (showahead_fifo_of_classic_fifo create_fifo)
  in
  fun ~capacity ~clock ~clear ~wr ~d ~rd ->
    let write_clock = clock in
    let read_clock = clock in
    match create_showahead_fifo ~capacity ~write_clock ~read_clock ~clear ~wr ~d ~rd with
    | Kinded_fifo.Showahead fifo -> fifo
;;

let create_showahead_with_read_latency
  ~read_latency
  ?nearly_empty
  ?nearly_full
  ?(overflow_check = true)
  ?(underflow_check = true)
  ?ram_attributes
  ?scope
  ()
  ~capacity
  ~clock
  ~clear
  ~wr
  ~d
  ~rd
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let fifo_rd_en = wire 1 in
  let full', empty' = wire 1, wire 1 in
  let rd = if underflow_check then rd &: ~:empty' else rd in
  let wr = if overflow_check then wr &: ~:full' else wr in
  let fifo =
    create
      ~showahead:false
      ~read_latency
      ?nearly_empty
      ?nearly_full
      ?ram_attributes
      ?scope
      ()
      ~overflow_check
      ~underflow_check
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd:fifo_rd_en
  in
  let read_pipeline = pipeline spec fifo_rd_en ~n:read_latency in
  let read_in_progress =
    reg_fb spec ~width:1 ~f:(fun q -> mux2 read_pipeline gnd (fifo_rd_en |: q))
  in
  let dout_valid =
    reg_fb spec ~width:1 ~f:(fun q -> mux2 (rd &: q) gnd (q |: read_pipeline))
  in
  let dout = reg ~enable:(~:dout_valid |: (dout_valid &: rd)) spec fifo.q in
  fifo_rd_en
  <-- (~:(fifo.empty) &: ~:read_in_progress &: (~:dout_valid |: (dout_valid &: rd)));
  let module Fifo_helper =
    Fifo_helper (struct
      let used_bits = num_bits_to_represent capacity
      let actual_capacity = capacity
    end)
  in
  let%tydi { used; used_next = _; empty = _; nearly_empty; full; nearly_full = _ } =
    Fifo_helper.create ?nearly_full ?nearly_empty ?scope { clock; clear; wr; rd }
  in
  empty' <-- ~:dout_valid;
  full' <-- full;
  { fifo with q = dout; empty = empty'; used; nearly_empty; full }
;;

type 'a showahead_with_extra_reg =
  { fifo : 'a t
  ; fifo_rd_en : 'a
  }

let create_showahead_with_extra_reg_wrapper
  ?(nearly_empty = 1)
  ?nearly_full
  ?scope
  fifo
  ~overflow_check
  ~underflow_check
  ~capacity
  ~clock
  ~clear
  ~wr
  ~rd
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let fifo_rd_en = wire 1 in
  let full', empty' = wire 1, wire 1 in
  let rd = if underflow_check then rd &: ~:empty' else rd in
  let wr = if overflow_check then wr &: ~:full' else wr in
  let fifo_valid = wire 1 in
  let middle_valid = wire 1 in
  let dout_valid = wire 1 in
  let will_update_dout = middle_valid |: fifo_valid &: (rd |: ~:dout_valid) in
  let will_update_middle = fifo_valid &: (middle_valid ==: will_update_dout) in
  let middle_dout = reg spec ~enable:will_update_middle fifo.q in
  let dout = reg spec ~enable:will_update_dout (mux2 middle_valid middle_dout fifo.q) in
  fifo_rd_en <-- (~:(fifo.empty) &: ~:(middle_valid &: dout_valid &: fifo_valid));
  fifo_valid
  <-- reg spec ~enable:(fifo_rd_en |: will_update_middle |: will_update_dout) fifo_rd_en;
  middle_valid
  <-- reg spec ~enable:(will_update_middle |: will_update_dout) will_update_middle;
  dout_valid <-- reg spec ~enable:(will_update_dout |: rd) will_update_dout;
  (* We re-instantiate a Fifo_helper as some of the capacity values will be different with
     the extra output registers. We derive the [empty] signal from the output register
     valid. *)
  let module Fifo_helper =
    Fifo_helper (struct
      (* Here we could add +2, but usually capacity is sized to a power of 2 to fit into
         RAM primitives, so taking 2 away from the FIFO probably won't make a difference.
      *)
      let used_bits = num_bits_to_represent capacity
      let actual_capacity = capacity
    end)
  in
  let%tydi { used; used_next = _; empty = _; nearly_empty; full; nearly_full } =
    Fifo_helper.create ?scope ?nearly_full ~nearly_empty { clock; clear; wr; rd }
  in
  empty' <-- ~:dout_valid;
  full' <-- full;
  { fifo = { fifo with q = dout; empty = empty'; nearly_empty; used; full; nearly_full }
  ; fifo_rd_en
  }
;;

let create_showahead_with_extra_reg
  ?(nearly_empty = 1)
  ?nearly_full
  ?(overflow_check = true)
  ?(underflow_check = true)
  ?ram_attributes
  ?scope
  ()
  ~capacity
  ~clock
  ~clear
  ~wr
  ~d
  ~rd
  =
  let fifo_rd_en' = wire 1 in
  let fifo =
    create
      ~showahead:false
      ?nearly_full
      ?ram_attributes
      ?scope
      ()
      ~overflow_check
      ~underflow_check
      ~nearly_empty
      ~capacity
      ~clock
      ~clear
      ~wr
      ~d
      ~rd:fifo_rd_en'
  in
  let { fifo; fifo_rd_en } =
    create_showahead_with_extra_reg_wrapper
      ~nearly_empty
      ?nearly_full
      ?scope
      fifo
      ~overflow_check
      ~underflow_check
      ~capacity
      ~clock
      ~clear
      ~wr
      ~rd
  in
  fifo_rd_en' <-- fifo_rd_en;
  fifo
;;

module Clocked = struct
  let create
    ?read_latency
    ?showahead
    ?nearly_empty
    ?nearly_full
    ?overflow_check
    ?underflow_check
    ?ram_attributes
    ?scope
    ()
    ~capacity
    ~clock
    ~clear
    ~wr
    ~d
    ~rd
    =
    let dom = Clocked_signal.get_domain clock in
    app
      (create
         ?read_latency
         ?showahead
         ?nearly_empty
         ?nearly_full
         ?overflow_check
         ?underflow_check
         ?ram_attributes
         ?scope
         ()
         ~capacity)
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
      ~f:(Clocked_signal.unwrap_signal ~dom)
    |> map ~f:(Clocked_signal.to_clocked ~dom)
  ;;

  let create_classic_with_extra_reg
    ?nearly_empty
    ?nearly_full
    ?overflow_check
    ?underflow_check
    ?ram_attributes
    ?scope
    ()
    ~capacity
    ~clock
    ~clear
    ~wr
    ~d
    ~rd
    =
    let dom = Clocked_signal.get_domain clock in
    app
      (create_classic_with_extra_reg
         ?nearly_empty
         ?nearly_full
         ?overflow_check
         ?underflow_check
         ?ram_attributes
         ?scope
         ()
         ~capacity)
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
      ~f:(Clocked_signal.unwrap_signal ~dom)
    |> map ~f:(Clocked_signal.to_clocked ~dom)
  ;;

  let create_showahead_from_classic
    ?nearly_empty
    ?nearly_full
    ?overflow_check
    ?underflow_check
    ?ram_attributes
    ?scope
    ()
    ~capacity
    ~clock
    ~clear
    ~wr
    ~d
    ~rd
    =
    let dom = Clocked_signal.get_domain clock in
    app
      (create_showahead_from_classic
         ?nearly_empty
         ?nearly_full
         ?overflow_check
         ?underflow_check
         ?ram_attributes
         ?scope
         ()
         ~capacity)
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
      ~f:(Clocked_signal.unwrap_signal ~dom)
    |> map ~f:(Clocked_signal.to_clocked ~dom)
  ;;

  let create_showahead_with_read_latency
    ~read_latency
    ?nearly_empty
    ?nearly_full
    ?overflow_check
    ?underflow_check
    ?ram_attributes
    ?scope
    ()
    ~capacity
    ~clock
    ~clear
    ~wr
    ~d
    ~rd
    =
    let dom = Clocked_signal.get_domain clock in
    app
      (create_showahead_with_read_latency
         ~read_latency
         ?nearly_empty
         ?nearly_full
         ?overflow_check
         ?underflow_check
         ?ram_attributes
         ?scope
         ()
         ~capacity)
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
      ~f:(Clocked_signal.unwrap_signal ~dom)
    |> map ~f:(Clocked_signal.to_clocked ~dom)
  ;;

  let create_showahead_with_extra_reg_wrapper
    ?nearly_empty
    ?nearly_full
    ?scope
    fifo
    ~overflow_check
    ~underflow_check
    ~capacity
    ~clock
    ~clear
    ~wr
    ~rd
    =
    let dom = Clocked_signal.get_domain clock in
    let unwrap = Clocked_signal.unwrap_signal ~dom in
    let wrap = Clocked_signal.to_clocked ~dom in
    let { fifo; fifo_rd_en } =
      (create_showahead_with_extra_reg_wrapper
         ?nearly_empty
         ?nearly_full
         ?scope
         (map fifo ~f:unwrap)
         ~overflow_check
         ~underflow_check
         ~capacity)
        ~clock:(unwrap clock)
        ~clear:(unwrap clear)
        ~wr:(unwrap wr)
        ~rd:(unwrap rd)
    in
    { fifo = map fifo ~f:wrap; fifo_rd_en = wrap fifo_rd_en }
  ;;

  let create_showahead_with_extra_reg
    ?nearly_empty
    ?nearly_full
    ?overflow_check
    ?underflow_check
    ?ram_attributes
    ?scope
    ()
    ~capacity
    ~clock
    ~clear
    ~wr
    ~d
    ~rd
    =
    let dom = Clocked_signal.get_domain clock in
    app
      (create_showahead_with_extra_reg
         ?nearly_empty
         ?nearly_full
         ?overflow_check
         ?underflow_check
         ?ram_attributes
         ?scope
         ()
         ~capacity)
      ~clock
      ~clear
      ~wr
      ~d
      ~rd
      ~f:(Clocked_signal.unwrap_signal ~dom)
    |> map ~f:(Clocked_signal.to_clocked ~dom)
  ;;
end

module type Config = Fifo_intf.Config

module With_interface (Config : Config) = struct
  let _actual_capacity, used_bits =
    capacity_and_used_bits Config.showahead Config.capacity
  ;;

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; wr : 'a
      ; d : 'a [@bits Config.data_width]
      ; rd : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type nonrec 'a t = 'a t

    include Interface.Make (struct
        include Fifo_intf.T

        let port_names_and_widths =
          { port_names_and_widths with
            q = "q", Config.data_width
          ; used = "used", used_bits
          }
        ;;
      end)
  end

  let create_fn
    ?nearly_empty
    ?nearly_full
    ?overflow_check
    ?underflow_check
    ?ram_attributes
    ?scope
    ~f
    (i : _ I.t)
    =
    f
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?underflow_check
      ?ram_attributes
      ?scope
      ()
      ~capacity:Config.capacity
      ~clock:i.clock
      ~clear:i.clear
      ~wr:i.wr
      ~d:i.d
      ~rd:i.rd
  ;;

  let classic ?(extra_reg = false) =
    match extra_reg, Config.showahead with
    | false, false -> create_fn ~f:(create ~showahead:false ?read_latency:None)
    | true, false -> create_fn ~f:create_classic_with_extra_reg
    | false, true -> create_fn ~f:create_showahead_from_classic
    | true, true -> create_fn ~f:create_showahead_with_extra_reg
  ;;

  let create = create_fn ~f:(create ~showahead:Config.showahead ?read_latency:None)
end

type 'a typed_fifo_read_result =
  { q : (Signal.t, 'a) With_valid.t2
  ; empty : Signal.t
  ; full : Signal.t
  ; nearly_empty : Signal.t
  ; nearly_full : Signal.t
  ; overflow : Signal.t
  ; read_when_empty : Signal.t
  ; used : Signal.t
  }

let typed_fifo
  (type a)
  ?nearly_empty
  ?nearly_full
  ?overflow_check
  ?underflow_check
  ?ram_attributes
  ?scope
  ~(clocking : Signal.t Clocking.t)
  ~capacity
  ~(input : (Signal.t, a) With_valid.t2)
  ~(read : Signal.t)
  (module C : Interface.S_Of_signal with type Of_signal.t = a)
  =
  let fifo =
    create
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?underflow_check
      ?ram_attributes
      ?scope
      ~showahead:true
      ~capacity
      ~clock:clocking.clock
      ~clear:clocking.clear
      ~wr:input.valid
      ~d:(C.Of_signal.pack input.value)
      ~rd:read
      ()
  in
  let unpacked_q = C.Of_signal.unpack fifo.q in
  { q = { With_valid.valid = ~:(fifo.empty); value = unpacked_q }
  ; empty = fifo.empty
  ; full = fifo.full
  ; nearly_empty = fifo.nearly_empty
  ; nearly_full = fifo.nearly_full
  ; overflow = fifo.full &: input.valid
  ; read_when_empty = fifo.empty &: read
  ; used = fifo.used
  }
;;

let cut_through_typed_fifo
  (type a)
  ?nearly_empty
  ?nearly_full
  ?overflow_check
  ?underflow_check
  ?ram_attributes
  ?scope
  ~(clocking : Signal.t Clocking.t)
  ~capacity
  ~(input : (Signal.t, a) With_valid.t2)
  ~(read : Signal.t)
  (module C : Interface.S_Of_signal with type Of_signal.t = a)
  =
  let cutting_through = wire 1 in
  let cutting_through_and_reading = wire 1 in
  let fifo_input = { input with valid = input.valid &: ~:cutting_through_and_reading } in
  let fifo_read = read &: ~:cutting_through in
  let underlying_fifo =
    typed_fifo
      ?nearly_empty
      ?nearly_full
      ?overflow_check
      ?underflow_check
      ?ram_attributes
      ?scope
      ~clocking
      ~capacity
      ~input:fifo_input
      ~read:fifo_read
      (module C)
  in
  cutting_through <-- (underlying_fifo.empty &: input.valid);
  cutting_through_and_reading <-- (cutting_through &: read);
  let q =
    { With_valid.valid = input.valid |: underlying_fifo.q.valid
    ; value = C.Of_signal.mux2 underlying_fifo.empty input.value underlying_fifo.q.value
    }
  in
  let empty = ~:(q.valid) in
  let nearly_empty =
    (* In the case that [nearly_empty] is 0, we cut it through like [empty]. *)
    match nearly_empty with
    | Some 0 -> empty
    | _ -> underlying_fifo.nearly_empty
  in
  { q
  ; empty
  ; full = underlying_fifo.full
  ; nearly_empty
  ; nearly_full = underlying_fifo.nearly_full
  ; overflow = underlying_fifo.overflow
  ; read_when_empty = empty &: read
  ; used = underlying_fifo.used
  }
;;
