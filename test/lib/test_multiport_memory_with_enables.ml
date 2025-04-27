open! Import
open! Hardcaml_waveterm_cyclesim

let write_port address_width data_width enable_width =
  { Write_port.write_clock = Signal.gnd
  ; write_address = Signal.of_int_trunc ~width:address_width 0
  ; write_data = Signal.of_int_trunc ~width:data_width 0
  ; write_enable = Signal.of_int_trunc ~width:enable_width 0
  }
;;

let%expect_test "ratio calculations and exceptions" =
  require_does_not_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:true
         16
         ~write_ports:[| write_port 4 8 1; write_port 4 8 2 |]
         ~read_addresses:[| Signal.zero 4 |]
       : Signal.t array));
  [%expect
    {|
    ((min_data_width 4) (max_num_enables 2)
     (write_data_and_enables
      (((data_width 8) (num_enables 1) (enable_replication 2))
       ((data_width 4) (num_enables 2) (enable_replication 1)))))
    |}];
  require_does_not_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:true
         16
         ~write_ports:[| write_port 4 18 9; write_port 4 18 6 |]
         ~read_addresses:[| Signal.zero 4 |]
       : Signal.t array));
  [%expect
    {|
    ((min_data_width 1) (max_num_enables 18)
     (write_data_and_enables
      (((data_width 2) (num_enables 9) (enable_replication 2))
       ((data_width 3) (num_enables 6) (enable_replication 3)))))
    |}];
  require_does_not_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:true
         16
         ~write_ports:[| write_port 4 18 2; write_port 4 18 3 |]
         ~read_addresses:[| Signal.zero 4 |]
       : Signal.t array));
  [%expect
    {|
    ((min_data_width 3) (max_num_enables 6)
     (write_data_and_enables
      (((data_width 9) (num_enables 2) (enable_replication 3))
       ((data_width 6) (num_enables 3) (enable_replication 2)))))
    |}];
  (* We support some pretty wild ratios for the data widths of enabled words ie below we
     can write in 3 and 4 bit chunks. *)
  require_does_not_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:true
         16
         ~write_ports:
           [| write_port 4 12 1
            ; write_port 4 12 2
            ; write_port 4 12 3
            ; write_port 4 12 4
            ; write_port 4 12 6
            ; write_port 4 12 12
           |]
         ~read_addresses:[| Signal.zero 4 |]
       : Signal.t array));
  [%expect
    {|
    ((min_data_width 1) (max_num_enables 12)
     (write_data_and_enables
      (((data_width 12) (num_enables 1) (enable_replication 12))
       ((data_width 6) (num_enables 2) (enable_replication 6))
       ((data_width 4) (num_enables 3) (enable_replication 4))
       ((data_width 3) (num_enables 4) (enable_replication 3))
       ((data_width 2) (num_enables 6) (enable_replication 2))
       ((data_width 1) (num_enables 12) (enable_replication 1)))))
    |}];
  (* Data and address widths must be consistent across ports. *)
  require_does_raise (fun () : Signal.t array ->
    Signal.multiport_memory
      ~enable_modelling_features:true
      ~verbose:true
      16
      ~write_ports:[| write_port 4 8 1; write_port 4 7 2 |]
      ~read_addresses:[| Signal.zero 4 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] Width of write port data and address are inconsistent with widest port"
     (write_port_data_width     7)
     (write_port_address_width  4)
     (ratio                     1)
     (widest_port_data_width    8)
     (widest_port_address_width 4))
    |}];
  require_does_raise (fun () : Signal.t array ->
    Signal.multiport_memory
      ~enable_modelling_features:true
      ~verbose:true
      16
      ~write_ports:[| write_port 4 8 1; write_port 5 8 2 |]
      ~read_addresses:[| Signal.zero 4 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] Width of write port data and address are inconsistent with widest port"
     (write_port_data_width     8)
     (write_port_address_width  5)
     (ratio                     2)
     (widest_port_data_width    8)
     (widest_port_address_width 4))
    |}];
  (* This is our main restriction - the number of enables must evenly divide the write
     port width. *)
  require_does_raise (fun () : Signal.t array ->
    Signal.multiport_memory
      ~enable_modelling_features:true
      ~verbose:true
      16
      ~write_ports:[| write_port 4 9 2; write_port 4 9 3 |]
      ~read_addresses:[| Signal.zero 4 |]);
  [%expect
    {|
    ("Write enables do not exactly divide the write data bus width"
     (data_width  9)
     (num_enables 2))
    |}];
  (* This calls the base generator without enables (so doesn't print on [verbose]) *)
  require_does_not_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~verbose:true
         16
         ~write_ports:[| write_port 4 8 1; write_port 4 8 1 |]
         ~read_addresses:[| Signal.zero 4 |]
       : Signal.t array));
  [%expect {| |}]
;;

let circ ~initialize_to ~address_width ~data_width ~enable_widths =
  let open Signal in
  let write_clock = input "write_clock" 1 in
  let write_port idx enable_width =
    let input n b = input (n ^ Int.to_string idx) b in
    { Write_port.write_clock
    ; write_address = input "write_address" address_width
    ; write_data = input "write_data" data_width
    ; write_enable = input "write_enable" enable_width
    }
  in
  let write_ports = Array.mapi enable_widths ~f:write_port in
  let q =
    Signal.multiport_memory
      ~enable_modelling_features:true
      ?initialize_to
      (Int.pow 2 address_width)
      ~write_ports
      ~read_addresses:[| input "read_address" address_width |]
  in
  let output i q = output ("q" ^ Int.to_string i) q in
  Circuit.create_exn ~name:"membe" (Array.mapi q ~f:output |> Array.to_list)
;;

let get_write_port sim idx =
  { Write_port.write_clock = ref Bits.gnd
  ; write_address = Cyclesim.in_port sim ("write_address" ^ Int.to_string idx)
  ; write_data = Cyclesim.in_port sim ("write_data" ^ Int.to_string idx)
  ; write_enable = Cyclesim.in_port sim ("write_enable" ^ Int.to_string idx)
  }
;;

let sim ~initialize_to ~address_width ~data_width ~enable_widths =
  let open Bits in
  let circ = circ ~initialize_to ~address_width ~data_width ~enable_widths in
  let sim = Cyclesim.create circ in
  let waves, sim = Waveform.create sim in
  let write_ports = Array.init (Array.length enable_widths) ~f:(get_write_port sim) in
  let read_address = Cyclesim.in_port sim "read_address" in
  let q = Cyclesim.out_port sim "q0" in
  let write idx address data enable =
    write_ports.(idx).write_address <--. address;
    write_ports.(idx).write_data <--. data;
    write_ports.(idx).write_enable <--. enable
  in
  let cycle () =
    Cyclesim.cycle sim;
    write_ports.(0).write_enable <--. 0;
    write_ports.(1).write_enable <--. 0
  in
  waves, cycle, write, read_address, q
;;

let%expect_test "simulate - simple byte enables" =
  let open Bits in
  let waves, cycle, write, read_address, _q =
    sim ~initialize_to:None ~address_width:9 ~data_width:32 ~enable_widths:[| 4; 4 |]
  in
  read_address <--. 1;
  write 0 1 0x3eadbeef 0b0001;
  write 1 2 0x12345678 0b1001;
  cycle ();
  write 0 1 0x3eadbeef 0b0010;
  cycle ();
  write 0 1 0x3eadbeef 0b0100;
  cycle ();
  write 0 1 0x3eadbeef 0b1000;
  cycle ();
  cycle ();
  read_address <--. 2;
  cycle ();
  cycle ();
  print_endline "";
  Waveform.print ~wave_width:4 ~display_width:90 waves;
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│                  ││──────────────────────────────────────────────────┬─────────────────│
│read_address      ││ 001                                              │002              │
│                  ││──────────────────────────────────────────────────┴─────────────────│
│                  ││────────────────────────────────────────────────────────────────────│
│write_address0    ││ 001                                                                │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││────────────────────────────────────────────────────────────────────│
│write_address1    ││ 002                                                                │
│                  ││────────────────────────────────────────────────────────────────────│
│write_clock       ││                                                                    │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││────────────────────────────────────────────────────────────────────│
│write_data0       ││ 3EADBEEF                                                           │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││────────────────────────────────────────────────────────────────────│
│write_data1       ││ 12345678                                                           │
│                  ││────────────────────────────────────────────────────────────────────│
│                  ││──────────┬─────────┬─────────┬─────────┬───────────────────────────│
│write_enable0     ││ 1        │2        │4        │8        │0                          │
│                  ││──────────┴─────────┴─────────┴─────────┴───────────────────────────│
│                  ││──────────┬─────────────────────────────────────────────────────────│
│write_enable1     ││ 9        │0                                                        │
│                  ││──────────┴─────────────────────────────────────────────────────────│
│                  ││──────────┬─────────┬─────────┬─────────┬─────────┬─────────────────│
│q0                ││ 00000000 │000000EF │0000BEEF │00ADBEEF │3EADBEEF │12000078         │
│                  ││──────────┴─────────┴─────────┴─────────┴─────────┴─────────────────│
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
|}]
;;

let%expect_test "simulate - squirly enable ratios" =
  let open Bits in
  let waves, cycle, write, read_address, _q =
    sim ~initialize_to:None ~address_width:3 ~data_width:24 ~enable_widths:[| 2; 3 |]
  in
  read_address <--. 7;
  write 0 7 0x654321 0b11;
  cycle ();
  write 0 7 0xabcfff 0b10;
  cycle ();
  write 1 7 0x009800 0b010;
  cycle ();
  write 1 7 0xFF00EE 0b101;
  cycle ();
  cycle ();
  print_endline "";
  Waveform.print ~wave_width:4 ~display_width:90 waves;
  [%expect_exact
    {|
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
│                  ││──────────────────────────────────────────────────                  │
│read_address      ││ 7                                                                  │
│                  ││──────────────────────────────────────────────────                  │
│                  ││──────────────────────────────────────────────────                  │
│write_address0    ││ 7                                                                  │
│                  ││──────────────────────────────────────────────────                  │
│                  ││────────────────────┬─────────────────────────────                  │
│write_address1    ││ 0                  │7                                              │
│                  ││────────────────────┴─────────────────────────────                  │
│write_clock       ││                                                                    │
│                  ││──────────────────────────────────────────────────                  │
│                  ││──────────┬───────────────────────────────────────                  │
│write_data0       ││ 654321   │ABCFFF                                                   │
│                  ││──────────┴───────────────────────────────────────                  │
│                  ││────────────────────┬─────────┬───────────────────                  │
│write_data1       ││ 000000             │009800   │FF00EE                               │
│                  ││────────────────────┴─────────┴───────────────────                  │
│                  ││──────────┬─────────┬─────────────────────────────                  │
│write_enable0     ││ 3        │2        │0                                              │
│                  ││──────────┴─────────┴─────────────────────────────                  │
│                  ││────────────────────┬─────────┬─────────┬─────────                  │
│write_enable1     ││ 0                  │2        │5        │0                          │
│                  ││────────────────────┴─────────┴─────────┴─────────                  │
│                  ││──────────┬─────────┬─────────┬─────────┬─────────                  │
│q0                ││ 000000   │654321   │ABC321   │AB9821   │FF98EE                     │
│                  ││──────────┴─────────┴─────────┴─────────┴─────────                  │
└──────────────────┘└────────────────────────────────────────────────────────────────────┘
|}]
;;

let ram_model ~initialize_to ~data_width ~enable_widths =
  let memory = Array.copy initialize_to in
  let bits_per_enable =
    Array.map enable_widths ~f:(fun enable_width -> data_width / enable_width)
  in
  let read address = memory.(address) in
  let write port address data enable =
    let enable_width = enable_widths.(port) in
    let data = Bits.of_int_trunc ~width:data_width data in
    let enable = Bits.of_int_trunc ~width:enable_width enable in
    let mask =
      Bits.bits_lsb enable
      |> List.map ~f:(fun b -> Bits.repeat b ~count:bits_per_enable.(port))
      |> Bits.concat_lsb
    in
    memory.(address) <- Bits.(memory.(address) &: ~:mask |: (data &: mask))
  in
  read, write
;;

(* Perform 10 write, then read back the RAM from the hardware and model and ensure they
   match. Repeat 10 times. Data is pre-initialized. *)
let test_randomly ~address_width ~data_width ~enable_widths =
  let open Bits in
  let size = Int.pow 2 address_width in
  let initialize_to = Array.init size ~f:(fun _ -> random ~width:data_width) in
  let _, cycle, write_sim, read_address, q =
    sim ~initialize_to:(Some initialize_to) ~address_width ~data_width ~enable_widths
  in
  let read_model, write_model = ram_model ~initialize_to ~data_width ~enable_widths in
  for _ = 0 to 9 do
    (* perform 10 random writes *)
    for _ = 0 to 10 do
      let data = Random.int (Int.pow 2 data_width) in
      let address = Random.int (Int.pow 2 address_width) in
      let port = Random.int (Array.length enable_widths) in
      let enable = Random.int (Int.pow 2 enable_widths.(port)) in
      write_model port address data enable;
      write_sim port address data enable;
      cycle ()
    done;
    for address = 0 to size - 1 do
      read_address <--. address;
      cycle ();
      let model = read_model address in
      if not (equal model !q)
      then
        raise_s
          [%message "MISMATCH" (address : int) (model : Bits.Hex.t) (!q : Bits.Hex.t)]
    done
  done
;;

let%expect_test "randomly against a model" =
  test_randomly ~address_width:4 ~data_width:16 ~enable_widths:[| 2; 4 |];
  test_randomly ~address_width:5 ~data_width:18 ~enable_widths:[| 2; 1 |];
  test_randomly ~address_width:6 ~data_width:24 ~enable_widths:[| 2; 3 |]
;;

let%expect_test "show verilog" =
  circ ~initialize_to:None ~address_width:9 ~data_width:32 ~enable_widths:[| 4; 1 |]
  |> Rtl.print Verilog;
  [%expect
    {|
    module membe (
        write_enable1,
        write_data1,
        write_address1,
        write_enable0,
        write_data0,
        write_address0,
        write_clock,
        read_address,
        q0
    );

        input write_enable1;
        input [31:0] write_data1;
        input [8:0] write_address1;
        input [3:0] write_enable0;
        input [31:0] write_data0;
        input [8:0] write_address0;
        input write_clock;
        input [8:0] read_address;
        output [31:0] q0;

        wire [1:0] _48;
        wire [10:0] _49;
        wire [7:0] _50;
        wire [1:0] _45;
        wire [10:0] _46;
        wire [7:0] _47;
        wire [1:0] _42;
        wire [10:0] _43;
        wire [7:0] _44;
        wire [7:0] _39;
        wire [1:0] _37;
        wire [10:0] _38;
        wire [7:0] _36;
        wire [10:0] _35;
        wire [7:0] _33;
        wire [10:0] _32;
        wire [7:0] _30;
        wire [10:0] _29;
        wire _27;
        wire [7:0] _26;
        wire [10:0] _25;
        wire _23;
        wire [7:0] _22;
        wire [10:0] _21;
        wire _19;
        wire [7:0] _18;
        wire [10:0] _17;
        wire _15;
        wire [7:0] _14;
        wire [10:0] _13;
        reg [7:0] _40[0:2047];
        wire [10:0] _11;
        wire [7:0] _41;
        wire [31:0] _51;
        assign _48 = 2'b00;
        assign _49 = { read_address,
                       _48 };
        assign _50 = _40[_49];
        assign _45 = 2'b01;
        assign _46 = { read_address,
                       _45 };
        assign _47 = _40[_46];
        assign _42 = 2'b10;
        assign _43 = { read_address,
                       _42 };
        assign _44 = _40[_43];
        assign _39 = write_data1[31:24];
        assign _37 = 2'b11;
        assign _38 = { write_address1,
                       _37 };
        assign _36 = write_data1[23:16];
        assign _35 = { write_address1,
                       _42 };
        assign _33 = write_data1[15:8];
        assign _32 = { write_address1,
                       _45 };
        assign _30 = write_data1[7:0];
        assign _29 = { write_address1,
                       _48 };
        assign _27 = write_enable0[3:3];
        assign _26 = write_data0[31:24];
        assign _25 = { write_address0,
                       _37 };
        assign _23 = write_enable0[2:2];
        assign _22 = write_data0[23:16];
        assign _21 = { write_address0,
                       _42 };
        assign _19 = write_enable0[1:1];
        assign _18 = write_data0[15:8];
        assign _17 = { write_address0,
                       _45 };
        assign _15 = write_enable0[0:0];
        assign _14 = write_data0[7:0];
        assign _13 = { write_address0,
                       _48 };
        always @(posedge write_clock) begin
            if (_15)
                _40[_13] <= _14;
        end
        always @(posedge write_clock) begin
            if (_19)
                _40[_17] <= _18;
        end
        always @(posedge write_clock) begin
            if (_23)
                _40[_21] <= _22;
        end
        always @(posedge write_clock) begin
            if (_27)
                _40[_25] <= _26;
        end
        always @(posedge write_clock) begin
            if (write_enable1)
                _40[_29] <= _30;
        end
        always @(posedge write_clock) begin
            if (write_enable1)
                _40[_32] <= _33;
        end
        always @(posedge write_clock) begin
            if (write_enable1)
                _40[_35] <= _36;
        end
        always @(posedge write_clock) begin
            if (write_enable1)
                _40[_38] <= _39;
        end
        assign _11 = { read_address,
                       _37 };
        assign _41 = _40[_11];
        assign _51 = { _41,
                       _44,
                       _47,
                       _50 };
        assign q0 = _51;

    endmodule
    |}];
  (* With non-power of two enables we need to generate multipliers and adders to compute
     the address. But as shown above, we detect powers of two and generate shifts when
     possible. *)
  circ ~initialize_to:None ~address_width:9 ~data_width:9 ~enable_widths:[| 3 |]
  |> Rtl.print Verilog;
  [%expect
    {|
    module membe (
        write_enable0,
        write_data0,
        write_address0,
        write_clock,
        read_address,
        q0
    );

        input [2:0] write_enable0;
        input [8:0] write_data0;
        input [8:0] write_address0;
        input write_clock;
        input [8:0] read_address;
        output [8:0] q0;

        wire [1:0] _34;
        wire [10:0] _35;
        wire [2:0] _36;
        wire [10:0] _31;
        wire [10:0] _30;
        wire [10:0] _32;
        wire [2:0] _33;
        wire _26;
        wire [2:0] _25;
        wire [10:0] _23;
        wire [10:0] _22;
        wire [10:0] _24;
        wire _20;
        wire [2:0] _19;
        wire [10:0] _16;
        wire [10:0] _18;
        wire _14;
        wire [2:0] _13;
        wire [10:0] _12;
        reg [2:0] _27[0:1535];
        wire [10:0] _8;
        wire [10:0] _10;
        wire [2:0] _28;
        wire [8:0] _37;
        assign _34 = 2'b11;
        assign _35 = read_address * _34;
        assign _36 = _27[_35];
        assign _31 = 11'b00000000001;
        assign _30 = read_address * _34;
        assign _32 = _30 + _31;
        assign _33 = _27[_32];
        assign _26 = write_enable0[2:2];
        assign _25 = write_data0[8:6];
        assign _23 = 11'b00000000010;
        assign _22 = write_address0 * _34;
        assign _24 = _22 + _23;
        assign _20 = write_enable0[1:1];
        assign _19 = write_data0[5:3];
        assign _16 = write_address0 * _34;
        assign _18 = _16 + _31;
        assign _14 = write_enable0[0:0];
        assign _13 = write_data0[2:0];
        assign _12 = write_address0 * _34;
        always @(posedge write_clock) begin
            if (_14)
                _27[_12] <= _13;
        end
        always @(posedge write_clock) begin
            if (_20)
                _27[_18] <= _19;
        end
        always @(posedge write_clock) begin
            if (_26)
                _27[_24] <= _25;
        end
        assign _8 = read_address * _34;
        assign _10 = _8 + _23;
        assign _28 = _27[_10];
        assign _37 = { _28,
                       _33,
                       _36 };
        assign q0 = _37;

    endmodule
    |}]
;;
