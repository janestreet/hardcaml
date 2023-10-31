open Base
open Signal

let gray_inc_mux_arg bits =
  List.init (1 lsl bits) ~f:(fun i ->
    let i = Bits.of_int ~width:bits i in
    Bits.(binary_to_gray i, binary_to_gray (i +:. 1)))
;;

let%expect_test "binary_to_gray" =
  let to_str x = Bits.to_string x in
  List.iter (gray_inc_mux_arg 4) ~f:(fun (i, b) ->
    Stdio.printf "%s -> %s\n" (to_str i) (to_str b));
  [%expect
    {|
    0000 -> 0001
    0001 -> 0011
    0011 -> 0010
    0010 -> 0110
    0110 -> 0111
    0111 -> 0101
    0101 -> 0100
    0100 -> 1100
    1100 -> 1101
    1101 -> 1111
    1111 -> 1110
    1110 -> 1010
    1010 -> 1011
    1011 -> 1001
    1001 -> 1000
    1000 -> 0000 |}]
;;

let gray_inc bits =
  let gray_inc_mux_arg =
    gray_inc_mux_arg bits
    |> List.sort ~compare:(fun (a, _) (b, _) -> Bits.compare a b)
    |> List.map ~f:snd
    |> List.map ~f:(fun x -> Signal.of_constant (Bits.to_constant x))
  in
  fun x -> mux x gray_inc_mux_arg
;;

module type S = sig
  val width : int
  val log2_depth : int
end

module Make (M : sig
  val width : int
  val log2_depth : int
end) =
struct
  let address_width = M.log2_depth
  let fifo_capacity = 1 lsl address_width
  let gray_inc = gray_inc address_width

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
  end

  module O = struct
    type 'a t =
      { full : 'a
      ; data_out : 'a [@bits M.width]
      ; valid : 'a
      ; almost_empty : 'a
      }
    [@@deriving hardcaml]
  end

  module Async_distributed_ram = struct
    type t =
      { clock_write : Signal.t
      ; multiport_mem : Signal.t
      ; mutable is_written : bool
      ; mutable is_read : bool
      ; write_enable : Always.Variable.t
      ; write_address : Signal.t
      ; write_data : Signal.t
      ; read_address : Signal.t
      }

    let create ~name ~clock_write =
      let write_data = Signal.wire M.width in
      let write_address = Signal.wire address_width in
      let read_address = Signal.wire address_width in
      let write_enable = Always.Variable.wire ~default:Signal.gnd in
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
      assert (width address = address_width);
      if t.is_read then raise_s [%message "Async_ram has already previously been read"];
      t.is_read <- true;
      t.read_address <== address;
      t.multiport_mem
    ;;

    let write t ~address ~data =
      assert (width address = address_width);
      assert (width data = M.width);
      if t.is_written
      then raise_s [%message "Async_ram has already previously been written"];
      t.is_written <- true;
      t.write_address <== address;
      t.write_data <== data;
      Always.(t.write_enable <--. 1)
    ;;
  end

  let create ?scope (i : _ I.t) =
    let ( -- ) =
      match scope with
      | Some scope -> Scope.naming scope
      | None -> ( -- )
    in
    let async_reg_var ~name ~clock ~reset ~width =
      let var =
        Always.Variable.reg (Reg_spec.create ~clock ~reset ()) ~enable:vdd ~width
      in
      ignore
        (Signal.add_attribute var.value (Rtl_attribute.Vivado.async_reg true) -- name
          : Signal.t);
      var
    in
    let reg_var ~name ~clock ~reset ~width =
      let var =
        Always.Variable.reg (Reg_spec.create ~clock ~reset ()) ~enable:vdd ~width
      in
      ignore (var.value -- name : Signal.t);
      var
    in
    let waddr_rd =
      reg_var
        ~clock:i.clock_read
        ~reset:i.reset_read
        ~width:address_width
        ~name:"waddr_rd"
    in
    let waddr_rd_ff =
      async_reg_var
        ~name:"waddr_rd_ff"
        ~clock:i.clock_read
        ~reset:i.reset_read
        ~width:address_width
    in
    let raddr_rd =
      reg_var
        ~clock:i.clock_read
        ~reset:i.reset_read
        ~width:address_width
        ~name:"raddr_rd"
    in
    let data_out =
      reg_var ~clock:i.clock_read ~reset:i.reset_read ~width:M.width ~name:"data_out"
    in
    let waddr_wd =
      reg_var
        ~clock:i.clock_write
        ~reset:i.reset_write
        ~width:address_width
        ~name:"waddr_wd"
    in
    let raddr_wd_ff =
      async_reg_var
        ~name:"raddr_wd_ff"
        ~clock:i.clock_write
        ~reset:i.reset_write
        ~width:address_width
    in
    let raddr_wd =
      reg_var
        ~clock:i.clock_write
        ~reset:i.reset_write
        ~width:address_width
        ~name:"raddr_wd"
    in
    let full = gray_inc waddr_wd.value ==: raddr_wd.value in
    let vld = waddr_rd.value <>: raddr_rd.value in
    let almost_empty =
      waddr_rd.value
      ==: raddr_rd.value
      |: (waddr_rd.value ==: gray_inc raddr_rd.value)
      |: (waddr_rd.value ==: gray_inc (gray_inc raddr_rd.value))
    in
    let ram =
      Async_distributed_ram.create
        ~name:(Option.value_map scope ~default:"ram" ~f:(fun s -> Scope.name s "ram"))
        ~clock_write:i.clock_write
    in
    let raddr_rd_next =
      mux2 (i.read_enable &: vld) (gray_inc raddr_rd.value) raddr_rd.value
    in
    Always.(
      compile
        [ (* @(posedge clk_read) *)
          waddr_rd_ff <-- waddr_wd.value
        ; waddr_rd <-- waddr_rd_ff.value
        ; (* @(posedge clk_write) *)
          raddr_wd_ff <-- raddr_rd.value
        ; raddr_wd <-- raddr_wd_ff.value
        ; (* @(posedge clk_write) *)
          when_
            (i.write_enable &: ~:full)
            [ Async_distributed_ram.write ram ~address:waddr_wd.value ~data:i.data_in
            ; waddr_wd <-- gray_inc waddr_wd.value
            ]
        ; (* @(posedge clk_read) *)
          data_out <-- Async_distributed_ram.read ram ~address:raddr_rd_next
        ; (* @(posedge clk_read) *)
          raddr_rd <-- raddr_rd_next
        ]);
    { O.full; data_out = data_out.value; valid = vld; almost_empty }
  ;;

  let create_with_delay ?(delay = 0) scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let async_fifo_has_valid_value = wire 1 in
    (* If delay is 0 we don't count, otherwise start a counter each time we see valid low
       on the FIFO output, and only set it back to 0 when the FIFO valid goes low again *)
    let delay_val =
      if delay = 0
      then vdd
      else (
        let min_bits = Bits.num_bits_to_represent delay in
        let spec = Reg_spec.create () ~clock:i.clock_read ~reset:i.reset_read in
        let delay_cnt_wire = wire min_bits in
        let delay_cnt =
          reg_fb spec ~enable:vdd ~width:min_bits ~f:(fun d ->
            mux2
              async_fifo_has_valid_value
              (mux2 (delay_cnt_wire ==:. delay) d (d +:. 1))
              (zero min_bits))
        in
        delay_cnt_wire <== delay_cnt;
        delay_cnt ==:. delay)
    in
    let async_fifo =
      create
        ~scope
        { i with
          read_enable = (i.read_enable &: delay_val) -- "read_en"
        ; write_enable = i.write_enable -- "write_en"
        ; data_in = i.data_in -- "data_in"
        }
    in
    async_fifo_has_valid_value <== async_fifo.valid;
    { O.full = async_fifo.full -- "full"
    ; data_out = async_fifo.data_out
    ; valid = async_fifo.valid &: delay_val
    ; almost_empty = async_fifo.almost_empty
    }
  ;;

  let hierarchical ?(name = "hardcaml_async_fifo") scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name ~scope (fun scope -> create ~scope) i
  ;;

  let hierarchical_with_delay ?(name = "hardcaml_async_fifo_with_delay") ?delay scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~name ~scope (create_with_delay ?delay) i
  ;;
end
