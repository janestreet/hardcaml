(* Build adder circuits using NAND as the base gate. Demonstrates the use of the
   [Comb.Make_primitives] functor. *)

open! Import

module Make_nand_gates (X : sig
    module Bits : Comb.S

    val nand : Bits.t -> Bits.t -> Bits.t
  end) : Comb.Gates with type t = X.Bits.t = struct
  include X.Bits

  let ( ~: ) a = X.nand a a
  let ( &: ) a b = ~:(X.nand a b)
  let ( |: ) a b = X.nand ~:a ~:b

  let ( ^: ) a b =
    let c = X.nand a b in
    X.nand (X.nand a c) (X.nand b c)
  ;;
end

module Bits_nand = Comb.Make (Comb.Make_primitives (Make_nand_gates (struct
    module Bits = Bits

    let nand a b = Bits.(~:(a &: b))
  end)))

let%expect_test "3 bit adder, bits" =
  for a = 0 to 7 do
    print_s
      [%message
        ""
          ~_:
            (List.init 8 ~f:(fun b ->
               Bits_nand.(of_int_trunc ~width:3 a +: of_int_trunc ~width:3 b))
             : Bits.t list)]
  done;
  [%expect
    {|
    (000 001 010 011 100 101 110 111)
    (001 010 011 100 101 110 111 000)
    (010 011 100 101 110 111 000 001)
    (011 100 101 110 111 000 001 010)
    (100 101 110 111 000 001 010 011)
    (101 110 111 000 001 010 011 100)
    (110 111 000 001 010 011 100 101)
    (111 000 001 010 011 100 101 110)
    |}]
;;

module Asic_nand = Comb.Make (Comb.Make_primitives (Make_nand_gates (struct
    module Bits = Signal

    let nand a b =
      assert (Bits.width a = Bits.width b);
      Instantiation.output
        (Instantiation.create
           ()
           ~name:"nand"
           ~inputs:[ "a", a; "b", b ]
           ~outputs:[ "c", Bits.width a ])
        "c"
    ;;
  end)))

let%expect_test "3 bit adder, ASIC style, verilog" =
  let open Signal in
  let a, b = input "a" 3, input "b" 3 in
  let c = output "c" Asic_nand.(a +: b) in
  let circuit = Circuit.create_exn ~name:"adder_nand" [ c ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module adder_nand (
        b,
        a,
        c
    );

        input [2:0] b;
        input [2:0] a;
        output [2:0] c;

        wire signal_inst;
        wire signal_wire;
        wire signal_inst_1;
        wire signal_wire_1;
        wire signal_inst_2;
        wire signal_wire_2;
        wire signal_inst_3;
        wire signal_wire_3;
        wire signal_inst_4;
        wire signal_wire_4;
        wire signal_inst_5;
        wire signal_wire_5;
        wire signal_inst_6;
        wire signal_wire_6;
        wire signal_inst_7;
        wire signal_wire_7;
        wire signal_inst_8;
        wire signal_wire_8;
        wire signal_inst_9;
        wire signal_wire_9;
        wire signal_inst_10;
        wire signal_wire_10;
        wire signal_inst_11;
        wire signal_wire_11;
        wire signal_inst_12;
        wire signal_wire_12;
        wire signal_inst_13;
        wire signal_wire_13;
        wire signal_inst_14;
        wire signal_wire_14;
        wire signal_inst_15;
        wire signal_wire_15;
        wire signal_inst_16;
        wire signal_wire_16;
        wire signal_inst_17;
        wire signal_wire_17;
        wire signal_inst_18;
        wire signal_wire_18;
        wire signal_inst_19;
        wire signal_wire_19;
        wire signal_inst_20;
        wire signal_wire_20;
        wire signal_inst_21;
        wire signal_wire_21;
        wire signal_inst_22;
        wire signal_wire_22;
        wire gnd;
        wire signal_inst_23;
        wire signal_wire_23;
        wire signal_inst_24;
        wire signal_wire_24;
        wire signal_inst_25;
        wire signal_wire_25;
        wire signal_select;
        wire signal_select_1;
        wire signal_inst_26;
        wire signal_wire_26;
        wire signal_inst_27;
        wire signal_wire_27;
        wire signal_inst_28;
        wire signal_wire_28;
        wire signal_inst_29;
        wire signal_wire_29;
        wire signal_inst_30;
        wire signal_wire_30;
        wire signal_inst_31;
        wire signal_wire_31;
        wire signal_inst_32;
        wire signal_wire_32;
        wire signal_inst_33;
        wire signal_wire_33;
        wire signal_inst_34;
        wire signal_wire_34;
        wire signal_select_2;
        wire signal_select_3;
        wire signal_inst_35;
        wire signal_wire_35;
        wire signal_inst_36;
        wire signal_wire_36;
        wire signal_inst_37;
        wire signal_wire_37;
        wire signal_inst_38;
        wire signal_wire_38;
        wire signal_inst_39;
        wire signal_wire_39;
        wire signal_inst_40;
        wire signal_wire_40;
        wire signal_inst_41;
        wire signal_wire_41;
        wire signal_inst_42;
        wire signal_wire_42;
        wire signal_select_4;
        wire signal_inst_43;
        wire signal_wire_43;
        wire signal_select_5;
        wire signal_inst_44;
        wire signal_wire_44;
        wire signal_inst_45;
        wire signal_wire_45;
        wire signal_inst_46;
        wire signal_wire_46;
        wire signal_inst_47;
        wire signal_wire_47;
        wire [2:0] signal_cat;
        \nand
            the_nand
            ( .a(gnd),
              .b(signal_wire_1),
              .c(signal_inst) );
        assign signal_wire = signal_inst;
        \nand
            the_nand_1
            ( .a(signal_wire_5),
              .b(gnd),
              .c(signal_inst_1) );
        assign signal_wire_1 = signal_inst_1;
        \nand
            the_nand_2
            ( .a(signal_select),
              .b(signal_wire_3),
              .c(signal_inst_2) );
        assign signal_wire_2 = signal_inst_2;
        \nand
            the_nand_3
            ( .a(signal_select_1),
              .b(signal_select),
              .c(signal_inst_3) );
        assign signal_wire_3 = signal_inst_3;
        \nand
            the_nand_4
            ( .a(signal_select_1),
              .b(signal_wire_3),
              .c(signal_inst_4) );
        assign signal_wire_4 = signal_inst_4;
        \nand
            the_nand_5
            ( .a(signal_wire_4),
              .b(signal_wire_2),
              .c(signal_inst_5) );
        assign signal_wire_5 = signal_inst_5;
        \nand
            the_nand_6
            ( .a(signal_wire_5),
              .b(signal_wire_1),
              .c(signal_inst_6) );
        assign signal_wire_6 = signal_inst_6;
        \nand
            the_nand_7
            ( .a(signal_wire_6),
              .b(signal_wire),
              .c(signal_inst_7) );
        assign signal_wire_7 = signal_inst_7;
        \nand
            the_nand_8
            ( .a(signal_wire_31),
              .b(signal_wire_9),
              .c(signal_inst_8) );
        assign signal_wire_8 = signal_inst_8;
        \nand
            the_nand_9
            ( .a(signal_wire_13),
              .b(signal_wire_31),
              .c(signal_inst_9) );
        assign signal_wire_9 = signal_inst_9;
        \nand
            the_nand_10
            ( .a(signal_select_2),
              .b(signal_wire_11),
              .c(signal_inst_10) );
        assign signal_wire_10 = signal_inst_10;
        \nand
            the_nand_11
            ( .a(signal_select_3),
              .b(signal_select_2),
              .c(signal_inst_11) );
        assign signal_wire_11 = signal_inst_11;
        \nand
            the_nand_12
            ( .a(signal_select_3),
              .b(signal_wire_11),
              .c(signal_inst_12) );
        assign signal_wire_12 = signal_inst_12;
        \nand
            the_nand_13
            ( .a(signal_wire_12),
              .b(signal_wire_10),
              .c(signal_inst_13) );
        assign signal_wire_13 = signal_inst_13;
        \nand
            the_nand_14
            ( .a(signal_wire_13),
              .b(signal_wire_9),
              .c(signal_inst_14) );
        assign signal_wire_14 = signal_inst_14;
        \nand
            the_nand_15
            ( .a(signal_wire_14),
              .b(signal_wire_8),
              .c(signal_inst_15) );
        assign signal_wire_15 = signal_inst_15;
        \nand
            the_nand_16
            ( .a(signal_wire_40),
              .b(signal_wire_41),
              .c(signal_inst_16) );
        assign signal_wire_16 = signal_inst_16;
        \nand
            the_nand_17
            ( .a(signal_wire_31),
              .b(signal_select_3),
              .c(signal_inst_17) );
        assign signal_wire_17 = signal_inst_17;
        \nand
            the_nand_18
            ( .a(signal_wire_17),
              .b(signal_wire_17),
              .c(signal_inst_18) );
        assign signal_wire_18 = signal_inst_18;
        \nand
            the_nand_19
            ( .a(signal_wire_18),
              .b(signal_wire_18),
              .c(signal_inst_19) );
        assign signal_wire_19 = signal_inst_19;
        \nand
            the_nand_20
            ( .a(gnd),
              .b(signal_select_1),
              .c(signal_inst_20) );
        assign signal_wire_20 = signal_inst_20;
        \nand
            the_nand_21
            ( .a(signal_wire_20),
              .b(signal_wire_20),
              .c(signal_inst_21) );
        assign signal_wire_21 = signal_inst_21;
        \nand
            the_nand_22
            ( .a(signal_wire_21),
              .b(signal_wire_21),
              .c(signal_inst_22) );
        assign signal_wire_22 = signal_inst_22;
        assign gnd = 1'b0;
        \nand
            the_nand_23
            ( .a(signal_select),
              .b(gnd),
              .c(signal_inst_23) );
        assign signal_wire_23 = signal_inst_23;
        \nand
            the_nand_24
            ( .a(signal_wire_23),
              .b(signal_wire_23),
              .c(signal_inst_24) );
        assign signal_wire_24 = signal_inst_24;
        \nand
            the_nand_25
            ( .a(signal_wire_24),
              .b(signal_wire_24),
              .c(signal_inst_25) );
        assign signal_wire_25 = signal_inst_25;
        assign signal_select = b[0:0];
        assign signal_select_1 = a[0:0];
        \nand
            the_nand_26
            ( .a(signal_select_1),
              .b(signal_select),
              .c(signal_inst_26) );
        assign signal_wire_26 = signal_inst_26;
        \nand
            the_nand_27
            ( .a(signal_wire_26),
              .b(signal_wire_26),
              .c(signal_inst_27) );
        assign signal_wire_27 = signal_inst_27;
        \nand
            the_nand_28
            ( .a(signal_wire_27),
              .b(signal_wire_27),
              .c(signal_inst_28) );
        assign signal_wire_28 = signal_inst_28;
        \nand
            the_nand_29
            ( .a(signal_wire_28),
              .b(signal_wire_25),
              .c(signal_inst_29) );
        assign signal_wire_29 = signal_inst_29;
        \nand
            the_nand_30
            ( .a(signal_wire_29),
              .b(signal_wire_29),
              .c(signal_inst_30) );
        assign signal_wire_30 = signal_inst_30;
        \nand
            the_nand_31
            ( .a(signal_wire_30),
              .b(signal_wire_22),
              .c(signal_inst_31) );
        assign signal_wire_31 = signal_inst_31;
        \nand
            the_nand_32
            ( .a(signal_select_2),
              .b(signal_wire_31),
              .c(signal_inst_32) );
        assign signal_wire_32 = signal_inst_32;
        \nand
            the_nand_33
            ( .a(signal_wire_32),
              .b(signal_wire_32),
              .c(signal_inst_33) );
        assign signal_wire_33 = signal_inst_33;
        \nand
            the_nand_34
            ( .a(signal_wire_33),
              .b(signal_wire_33),
              .c(signal_inst_34) );
        assign signal_wire_34 = signal_inst_34;
        assign signal_select_2 = b[1:1];
        assign signal_select_3 = a[1:1];
        \nand
            the_nand_35
            ( .a(signal_select_3),
              .b(signal_select_2),
              .c(signal_inst_35) );
        assign signal_wire_35 = signal_inst_35;
        \nand
            the_nand_36
            ( .a(signal_wire_35),
              .b(signal_wire_35),
              .c(signal_inst_36) );
        assign signal_wire_36 = signal_inst_36;
        \nand
            the_nand_37
            ( .a(signal_wire_36),
              .b(signal_wire_36),
              .c(signal_inst_37) );
        assign signal_wire_37 = signal_inst_37;
        \nand
            the_nand_38
            ( .a(signal_wire_37),
              .b(signal_wire_34),
              .c(signal_inst_38) );
        assign signal_wire_38 = signal_inst_38;
        \nand
            the_nand_39
            ( .a(signal_wire_38),
              .b(signal_wire_38),
              .c(signal_inst_39) );
        assign signal_wire_39 = signal_inst_39;
        \nand
            the_nand_40
            ( .a(signal_wire_39),
              .b(signal_wire_19),
              .c(signal_inst_40) );
        assign signal_wire_40 = signal_inst_40;
        \nand
            the_nand_41
            ( .a(signal_wire_45),
              .b(signal_wire_40),
              .c(signal_inst_41) );
        assign signal_wire_41 = signal_inst_41;
        \nand
            the_nand_42
            ( .a(signal_select_4),
              .b(signal_wire_43),
              .c(signal_inst_42) );
        assign signal_wire_42 = signal_inst_42;
        assign signal_select_4 = b[2:2];
        \nand
            the_nand_43
            ( .a(signal_select_5),
              .b(signal_select_4),
              .c(signal_inst_43) );
        assign signal_wire_43 = signal_inst_43;
        assign signal_select_5 = a[2:2];
        \nand
            the_nand_44
            ( .a(signal_select_5),
              .b(signal_wire_43),
              .c(signal_inst_44) );
        assign signal_wire_44 = signal_inst_44;
        \nand
            the_nand_45
            ( .a(signal_wire_44),
              .b(signal_wire_42),
              .c(signal_inst_45) );
        assign signal_wire_45 = signal_inst_45;
        \nand
            the_nand_46
            ( .a(signal_wire_45),
              .b(signal_wire_41),
              .c(signal_inst_46) );
        assign signal_wire_46 = signal_inst_46;
        \nand
            the_nand_47
            ( .a(signal_wire_46),
              .b(signal_wire_16),
              .c(signal_inst_47) );
        assign signal_wire_47 = signal_inst_47;
        assign signal_cat = { signal_wire_47,
                              signal_wire_15,
                              signal_wire_7 };
        assign c = signal_cat;

    endmodule
    |}]
;;

module Fpga_nand = Comb.Make (Comb.Make_primitives (Make_nand_gates (struct
    module Bits = Signal
    open Signal

    let nand_lut a b =
      Instantiation.output
        (Instantiation.create
           ()
           ~name:"LUT2"
           ~parameters:[ Parameter.create ~name:"INIT" ~value:(String "1110") ]
           ~inputs:[ "I", a @: b ]
           ~outputs:[ "O", 1 ])
        "O"
    ;;

    let nand a b =
      assert (width a = width b);
      concat_msb (List.map2_exn (bits_msb a) (bits_msb b) ~f:nand_lut)
    ;;
  end)))

let%expect_test "3 bit adder, FPGA style, verilog" =
  (* Not sayin' this is an efficient way to go about this... *)
  let open Signal in
  let a, b = input "a" 3, input "b" 3 in
  let c = output "c" Fpga_nand.(a +: b) in
  let circuit = Circuit.create_exn ~name:"adder_nand" [ c ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module adder_nand (
        b,
        a,
        c
    );

        input [2:0] b;
        input [2:0] a;
        output [2:0] c;

        wire [1:0] signal_cat;
        wire signal_inst;
        wire signal_wire;
        wire [1:0] signal_cat_1;
        wire signal_inst_1;
        wire signal_wire_1;
        wire [1:0] signal_cat_2;
        wire signal_inst_2;
        wire signal_wire_2;
        wire [1:0] signal_cat_3;
        wire signal_inst_3;
        wire signal_wire_3;
        wire [1:0] signal_cat_4;
        wire signal_inst_4;
        wire signal_wire_4;
        wire [1:0] signal_cat_5;
        wire signal_inst_5;
        wire signal_wire_5;
        wire [1:0] signal_cat_6;
        wire signal_inst_6;
        wire signal_wire_6;
        wire [1:0] signal_cat_7;
        wire signal_inst_7;
        wire signal_wire_7;
        wire [1:0] signal_cat_8;
        wire signal_inst_8;
        wire signal_wire_8;
        wire [1:0] signal_cat_9;
        wire signal_inst_9;
        wire signal_wire_9;
        wire [1:0] signal_cat_10;
        wire signal_inst_10;
        wire signal_wire_10;
        wire [1:0] signal_cat_11;
        wire signal_inst_11;
        wire signal_wire_11;
        wire [1:0] signal_cat_12;
        wire signal_inst_12;
        wire signal_wire_12;
        wire [1:0] signal_cat_13;
        wire signal_inst_13;
        wire signal_wire_13;
        wire [1:0] signal_cat_14;
        wire signal_inst_14;
        wire signal_wire_14;
        wire [1:0] signal_cat_15;
        wire signal_inst_15;
        wire signal_wire_15;
        wire [1:0] signal_cat_16;
        wire signal_inst_16;
        wire signal_wire_16;
        wire [1:0] signal_cat_17;
        wire signal_inst_17;
        wire signal_wire_17;
        wire [1:0] signal_cat_18;
        wire signal_inst_18;
        wire signal_wire_18;
        wire [1:0] signal_cat_19;
        wire signal_inst_19;
        wire signal_wire_19;
        wire [1:0] signal_cat_20;
        wire signal_inst_20;
        wire signal_wire_20;
        wire [1:0] signal_cat_21;
        wire signal_inst_21;
        wire signal_wire_21;
        wire [1:0] signal_cat_22;
        wire signal_inst_22;
        wire signal_wire_22;
        wire gnd;
        wire [1:0] signal_cat_23;
        wire signal_inst_23;
        wire signal_wire_23;
        wire [1:0] signal_cat_24;
        wire signal_inst_24;
        wire signal_wire_24;
        wire [1:0] signal_cat_25;
        wire signal_inst_25;
        wire signal_wire_25;
        wire signal_select;
        wire signal_select_1;
        wire [1:0] signal_cat_26;
        wire signal_inst_26;
        wire signal_wire_26;
        wire [1:0] signal_cat_27;
        wire signal_inst_27;
        wire signal_wire_27;
        wire [1:0] signal_cat_28;
        wire signal_inst_28;
        wire signal_wire_28;
        wire [1:0] signal_cat_29;
        wire signal_inst_29;
        wire signal_wire_29;
        wire [1:0] signal_cat_30;
        wire signal_inst_30;
        wire signal_wire_30;
        wire [1:0] signal_cat_31;
        wire signal_inst_31;
        wire signal_wire_31;
        wire [1:0] signal_cat_32;
        wire signal_inst_32;
        wire signal_wire_32;
        wire [1:0] signal_cat_33;
        wire signal_inst_33;
        wire signal_wire_33;
        wire [1:0] signal_cat_34;
        wire signal_inst_34;
        wire signal_wire_34;
        wire signal_select_2;
        wire signal_select_3;
        wire [1:0] signal_cat_35;
        wire signal_inst_35;
        wire signal_wire_35;
        wire [1:0] signal_cat_36;
        wire signal_inst_36;
        wire signal_wire_36;
        wire [1:0] signal_cat_37;
        wire signal_inst_37;
        wire signal_wire_37;
        wire [1:0] signal_cat_38;
        wire signal_inst_38;
        wire signal_wire_38;
        wire [1:0] signal_cat_39;
        wire signal_inst_39;
        wire signal_wire_39;
        wire [1:0] signal_cat_40;
        wire signal_inst_40;
        wire signal_wire_40;
        wire [1:0] signal_cat_41;
        wire signal_inst_41;
        wire signal_wire_41;
        wire [1:0] signal_cat_42;
        wire signal_inst_42;
        wire signal_wire_42;
        wire signal_select_4;
        wire [1:0] signal_cat_43;
        wire signal_inst_43;
        wire signal_wire_43;
        wire signal_select_5;
        wire [1:0] signal_cat_44;
        wire signal_inst_44;
        wire signal_wire_44;
        wire [1:0] signal_cat_45;
        wire signal_inst_45;
        wire signal_wire_45;
        wire [1:0] signal_cat_46;
        wire signal_inst_46;
        wire signal_wire_46;
        wire [1:0] signal_cat_47;
        wire signal_inst_47;
        wire signal_wire_47;
        wire [2:0] signal_cat_48;
        assign signal_cat = { gnd,
                              signal_wire_1 };
        LUT2
            #( .INIT("1110") )
            the_LUT2
            ( .I(signal_cat),
              .O(signal_inst) );
        assign signal_wire = signal_inst;
        assign signal_cat_1 = { signal_wire_5,
                                gnd };
        LUT2
            #( .INIT("1110") )
            the_LUT2_1
            ( .I(signal_cat_1),
              .O(signal_inst_1) );
        assign signal_wire_1 = signal_inst_1;
        assign signal_cat_2 = { signal_select,
                                signal_wire_3 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_2
            ( .I(signal_cat_2),
              .O(signal_inst_2) );
        assign signal_wire_2 = signal_inst_2;
        assign signal_cat_3 = { signal_select_1,
                                signal_select };
        LUT2
            #( .INIT("1110") )
            the_LUT2_3
            ( .I(signal_cat_3),
              .O(signal_inst_3) );
        assign signal_wire_3 = signal_inst_3;
        assign signal_cat_4 = { signal_select_1,
                                signal_wire_3 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_4
            ( .I(signal_cat_4),
              .O(signal_inst_4) );
        assign signal_wire_4 = signal_inst_4;
        assign signal_cat_5 = { signal_wire_4,
                                signal_wire_2 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_5
            ( .I(signal_cat_5),
              .O(signal_inst_5) );
        assign signal_wire_5 = signal_inst_5;
        assign signal_cat_6 = { signal_wire_5,
                                signal_wire_1 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_6
            ( .I(signal_cat_6),
              .O(signal_inst_6) );
        assign signal_wire_6 = signal_inst_6;
        assign signal_cat_7 = { signal_wire_6,
                                signal_wire };
        LUT2
            #( .INIT("1110") )
            the_LUT2_7
            ( .I(signal_cat_7),
              .O(signal_inst_7) );
        assign signal_wire_7 = signal_inst_7;
        assign signal_cat_8 = { signal_wire_31,
                                signal_wire_9 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_8
            ( .I(signal_cat_8),
              .O(signal_inst_8) );
        assign signal_wire_8 = signal_inst_8;
        assign signal_cat_9 = { signal_wire_13,
                                signal_wire_31 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_9
            ( .I(signal_cat_9),
              .O(signal_inst_9) );
        assign signal_wire_9 = signal_inst_9;
        assign signal_cat_10 = { signal_select_2,
                                 signal_wire_11 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_10
            ( .I(signal_cat_10),
              .O(signal_inst_10) );
        assign signal_wire_10 = signal_inst_10;
        assign signal_cat_11 = { signal_select_3,
                                 signal_select_2 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_11
            ( .I(signal_cat_11),
              .O(signal_inst_11) );
        assign signal_wire_11 = signal_inst_11;
        assign signal_cat_12 = { signal_select_3,
                                 signal_wire_11 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_12
            ( .I(signal_cat_12),
              .O(signal_inst_12) );
        assign signal_wire_12 = signal_inst_12;
        assign signal_cat_13 = { signal_wire_12,
                                 signal_wire_10 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_13
            ( .I(signal_cat_13),
              .O(signal_inst_13) );
        assign signal_wire_13 = signal_inst_13;
        assign signal_cat_14 = { signal_wire_13,
                                 signal_wire_9 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_14
            ( .I(signal_cat_14),
              .O(signal_inst_14) );
        assign signal_wire_14 = signal_inst_14;
        assign signal_cat_15 = { signal_wire_14,
                                 signal_wire_8 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_15
            ( .I(signal_cat_15),
              .O(signal_inst_15) );
        assign signal_wire_15 = signal_inst_15;
        assign signal_cat_16 = { signal_wire_40,
                                 signal_wire_41 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_16
            ( .I(signal_cat_16),
              .O(signal_inst_16) );
        assign signal_wire_16 = signal_inst_16;
        assign signal_cat_17 = { signal_wire_31,
                                 signal_select_3 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_17
            ( .I(signal_cat_17),
              .O(signal_inst_17) );
        assign signal_wire_17 = signal_inst_17;
        assign signal_cat_18 = { signal_wire_17,
                                 signal_wire_17 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_18
            ( .I(signal_cat_18),
              .O(signal_inst_18) );
        assign signal_wire_18 = signal_inst_18;
        assign signal_cat_19 = { signal_wire_18,
                                 signal_wire_18 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_19
            ( .I(signal_cat_19),
              .O(signal_inst_19) );
        assign signal_wire_19 = signal_inst_19;
        assign signal_cat_20 = { gnd,
                                 signal_select_1 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_20
            ( .I(signal_cat_20),
              .O(signal_inst_20) );
        assign signal_wire_20 = signal_inst_20;
        assign signal_cat_21 = { signal_wire_20,
                                 signal_wire_20 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_21
            ( .I(signal_cat_21),
              .O(signal_inst_21) );
        assign signal_wire_21 = signal_inst_21;
        assign signal_cat_22 = { signal_wire_21,
                                 signal_wire_21 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_22
            ( .I(signal_cat_22),
              .O(signal_inst_22) );
        assign signal_wire_22 = signal_inst_22;
        assign gnd = 1'b0;
        assign signal_cat_23 = { signal_select,
                                 gnd };
        LUT2
            #( .INIT("1110") )
            the_LUT2_23
            ( .I(signal_cat_23),
              .O(signal_inst_23) );
        assign signal_wire_23 = signal_inst_23;
        assign signal_cat_24 = { signal_wire_23,
                                 signal_wire_23 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_24
            ( .I(signal_cat_24),
              .O(signal_inst_24) );
        assign signal_wire_24 = signal_inst_24;
        assign signal_cat_25 = { signal_wire_24,
                                 signal_wire_24 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_25
            ( .I(signal_cat_25),
              .O(signal_inst_25) );
        assign signal_wire_25 = signal_inst_25;
        assign signal_select = b[0:0];
        assign signal_select_1 = a[0:0];
        assign signal_cat_26 = { signal_select_1,
                                 signal_select };
        LUT2
            #( .INIT("1110") )
            the_LUT2_26
            ( .I(signal_cat_26),
              .O(signal_inst_26) );
        assign signal_wire_26 = signal_inst_26;
        assign signal_cat_27 = { signal_wire_26,
                                 signal_wire_26 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_27
            ( .I(signal_cat_27),
              .O(signal_inst_27) );
        assign signal_wire_27 = signal_inst_27;
        assign signal_cat_28 = { signal_wire_27,
                                 signal_wire_27 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_28
            ( .I(signal_cat_28),
              .O(signal_inst_28) );
        assign signal_wire_28 = signal_inst_28;
        assign signal_cat_29 = { signal_wire_28,
                                 signal_wire_25 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_29
            ( .I(signal_cat_29),
              .O(signal_inst_29) );
        assign signal_wire_29 = signal_inst_29;
        assign signal_cat_30 = { signal_wire_29,
                                 signal_wire_29 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_30
            ( .I(signal_cat_30),
              .O(signal_inst_30) );
        assign signal_wire_30 = signal_inst_30;
        assign signal_cat_31 = { signal_wire_30,
                                 signal_wire_22 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_31
            ( .I(signal_cat_31),
              .O(signal_inst_31) );
        assign signal_wire_31 = signal_inst_31;
        assign signal_cat_32 = { signal_select_2,
                                 signal_wire_31 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_32
            ( .I(signal_cat_32),
              .O(signal_inst_32) );
        assign signal_wire_32 = signal_inst_32;
        assign signal_cat_33 = { signal_wire_32,
                                 signal_wire_32 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_33
            ( .I(signal_cat_33),
              .O(signal_inst_33) );
        assign signal_wire_33 = signal_inst_33;
        assign signal_cat_34 = { signal_wire_33,
                                 signal_wire_33 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_34
            ( .I(signal_cat_34),
              .O(signal_inst_34) );
        assign signal_wire_34 = signal_inst_34;
        assign signal_select_2 = b[1:1];
        assign signal_select_3 = a[1:1];
        assign signal_cat_35 = { signal_select_3,
                                 signal_select_2 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_35
            ( .I(signal_cat_35),
              .O(signal_inst_35) );
        assign signal_wire_35 = signal_inst_35;
        assign signal_cat_36 = { signal_wire_35,
                                 signal_wire_35 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_36
            ( .I(signal_cat_36),
              .O(signal_inst_36) );
        assign signal_wire_36 = signal_inst_36;
        assign signal_cat_37 = { signal_wire_36,
                                 signal_wire_36 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_37
            ( .I(signal_cat_37),
              .O(signal_inst_37) );
        assign signal_wire_37 = signal_inst_37;
        assign signal_cat_38 = { signal_wire_37,
                                 signal_wire_34 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_38
            ( .I(signal_cat_38),
              .O(signal_inst_38) );
        assign signal_wire_38 = signal_inst_38;
        assign signal_cat_39 = { signal_wire_38,
                                 signal_wire_38 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_39
            ( .I(signal_cat_39),
              .O(signal_inst_39) );
        assign signal_wire_39 = signal_inst_39;
        assign signal_cat_40 = { signal_wire_39,
                                 signal_wire_19 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_40
            ( .I(signal_cat_40),
              .O(signal_inst_40) );
        assign signal_wire_40 = signal_inst_40;
        assign signal_cat_41 = { signal_wire_45,
                                 signal_wire_40 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_41
            ( .I(signal_cat_41),
              .O(signal_inst_41) );
        assign signal_wire_41 = signal_inst_41;
        assign signal_cat_42 = { signal_select_4,
                                 signal_wire_43 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_42
            ( .I(signal_cat_42),
              .O(signal_inst_42) );
        assign signal_wire_42 = signal_inst_42;
        assign signal_select_4 = b[2:2];
        assign signal_cat_43 = { signal_select_5,
                                 signal_select_4 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_43
            ( .I(signal_cat_43),
              .O(signal_inst_43) );
        assign signal_wire_43 = signal_inst_43;
        assign signal_select_5 = a[2:2];
        assign signal_cat_44 = { signal_select_5,
                                 signal_wire_43 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_44
            ( .I(signal_cat_44),
              .O(signal_inst_44) );
        assign signal_wire_44 = signal_inst_44;
        assign signal_cat_45 = { signal_wire_44,
                                 signal_wire_42 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_45
            ( .I(signal_cat_45),
              .O(signal_inst_45) );
        assign signal_wire_45 = signal_inst_45;
        assign signal_cat_46 = { signal_wire_45,
                                 signal_wire_41 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_46
            ( .I(signal_cat_46),
              .O(signal_inst_46) );
        assign signal_wire_46 = signal_inst_46;
        assign signal_cat_47 = { signal_wire_46,
                                 signal_wire_16 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_47
            ( .I(signal_cat_47),
              .O(signal_inst_47) );
        assign signal_wire_47 = signal_inst_47;
        assign signal_cat_48 = { signal_wire_47,
                                 signal_wire_15,
                                 signal_wire_7 };
        assign c = signal_cat_48;

    endmodule
    |}]
;;
