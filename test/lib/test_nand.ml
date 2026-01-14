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

        wire _53;
        wire _1;
        wire _54;
        wire _2;
        wire _56;
        wire _3;
        wire _58;
        wire _4;
        wire _59;
        wire _5;
        wire _60;
        wire _6;
        wire _61;
        wire _7;
        wire _62;
        wire _8;
        wire _63;
        wire _9;
        wire _64;
        wire _10;
        wire _66;
        wire _11;
        wire _68;
        wire _12;
        wire _69;
        wire _13;
        wire _70;
        wire _14;
        wire _71;
        wire _15;
        wire _72;
        wire _16;
        wire _73;
        wire _17;
        wire _74;
        wire _18;
        wire _75;
        wire _19;
        wire _76;
        wire _20;
        wire _77;
        wire _21;
        wire _78;
        wire _22;
        wire _79;
        wire _23;
        wire gnd;
        wire _80;
        wire _24;
        wire _81;
        wire _25;
        wire _82;
        wire _26;
        wire _55;
        wire _57;
        wire _83;
        wire _27;
        wire _84;
        wire _28;
        wire _85;
        wire _29;
        wire _86;
        wire _30;
        wire _87;
        wire _31;
        wire _88;
        wire _32;
        wire _89;
        wire _33;
        wire _90;
        wire _34;
        wire _91;
        wire _35;
        wire _65;
        wire _67;
        wire _92;
        wire _36;
        wire _93;
        wire _37;
        wire _94;
        wire _38;
        wire _95;
        wire _39;
        wire _96;
        wire _40;
        wire _97;
        wire _41;
        wire _98;
        wire _42;
        wire _100;
        wire _43;
        wire _99;
        wire _102;
        wire _45;
        wire _101;
        wire _103;
        wire _47;
        wire _104;
        wire _48;
        wire _105;
        wire _49;
        wire _106;
        wire _50;
        wire [2:0] _107;
        nand
            the_nand
            ( .a(gnd),
              .b(_2),
              .c(_53) );
        assign _1 = _53;
        nand
            the_nand_1
            ( .a(_6),
              .b(gnd),
              .c(_54) );
        assign _2 = _54;
        nand
            the_nand_2
            ( .a(_55),
              .b(_4),
              .c(_56) );
        assign _3 = _56;
        nand
            the_nand_3
            ( .a(_57),
              .b(_55),
              .c(_58) );
        assign _4 = _58;
        nand
            the_nand_4
            ( .a(_57),
              .b(_4),
              .c(_59) );
        assign _5 = _59;
        nand
            the_nand_5
            ( .a(_5),
              .b(_3),
              .c(_60) );
        assign _6 = _60;
        nand
            the_nand_6
            ( .a(_6),
              .b(_2),
              .c(_61) );
        assign _7 = _61;
        nand
            the_nand_7
            ( .a(_7),
              .b(_1),
              .c(_62) );
        assign _8 = _62;
        nand
            the_nand_8
            ( .a(_32),
              .b(_10),
              .c(_63) );
        assign _9 = _63;
        nand
            the_nand_9
            ( .a(_14),
              .b(_32),
              .c(_64) );
        assign _10 = _64;
        nand
            the_nand_10
            ( .a(_65),
              .b(_12),
              .c(_66) );
        assign _11 = _66;
        nand
            the_nand_11
            ( .a(_67),
              .b(_65),
              .c(_68) );
        assign _12 = _68;
        nand
            the_nand_12
            ( .a(_67),
              .b(_12),
              .c(_69) );
        assign _13 = _69;
        nand
            the_nand_13
            ( .a(_13),
              .b(_11),
              .c(_70) );
        assign _14 = _70;
        nand
            the_nand_14
            ( .a(_14),
              .b(_10),
              .c(_71) );
        assign _15 = _71;
        nand
            the_nand_15
            ( .a(_15),
              .b(_9),
              .c(_72) );
        assign _16 = _72;
        nand
            the_nand_16
            ( .a(_41),
              .b(_42),
              .c(_73) );
        assign _17 = _73;
        nand
            the_nand_17
            ( .a(_32),
              .b(_67),
              .c(_74) );
        assign _18 = _74;
        nand
            the_nand_18
            ( .a(_18),
              .b(_18),
              .c(_75) );
        assign _19 = _75;
        nand
            the_nand_19
            ( .a(_19),
              .b(_19),
              .c(_76) );
        assign _20 = _76;
        nand
            the_nand_20
            ( .a(gnd),
              .b(_57),
              .c(_77) );
        assign _21 = _77;
        nand
            the_nand_21
            ( .a(_21),
              .b(_21),
              .c(_78) );
        assign _22 = _78;
        nand
            the_nand_22
            ( .a(_22),
              .b(_22),
              .c(_79) );
        assign _23 = _79;
        assign gnd = 1'b0;
        nand
            the_nand_23
            ( .a(_55),
              .b(gnd),
              .c(_80) );
        assign _24 = _80;
        nand
            the_nand_24
            ( .a(_24),
              .b(_24),
              .c(_81) );
        assign _25 = _81;
        nand
            the_nand_25
            ( .a(_25),
              .b(_25),
              .c(_82) );
        assign _26 = _82;
        assign _55 = b[0:0];
        assign _57 = a[0:0];
        nand
            the_nand_26
            ( .a(_57),
              .b(_55),
              .c(_83) );
        assign _27 = _83;
        nand
            the_nand_27
            ( .a(_27),
              .b(_27),
              .c(_84) );
        assign _28 = _84;
        nand
            the_nand_28
            ( .a(_28),
              .b(_28),
              .c(_85) );
        assign _29 = _85;
        nand
            the_nand_29
            ( .a(_29),
              .b(_26),
              .c(_86) );
        assign _30 = _86;
        nand
            the_nand_30
            ( .a(_30),
              .b(_30),
              .c(_87) );
        assign _31 = _87;
        nand
            the_nand_31
            ( .a(_31),
              .b(_23),
              .c(_88) );
        assign _32 = _88;
        nand
            the_nand_32
            ( .a(_65),
              .b(_32),
              .c(_89) );
        assign _33 = _89;
        nand
            the_nand_33
            ( .a(_33),
              .b(_33),
              .c(_90) );
        assign _34 = _90;
        nand
            the_nand_34
            ( .a(_34),
              .b(_34),
              .c(_91) );
        assign _35 = _91;
        assign _65 = b[1:1];
        assign _67 = a[1:1];
        nand
            the_nand_35
            ( .a(_67),
              .b(_65),
              .c(_92) );
        assign _36 = _92;
        nand
            the_nand_36
            ( .a(_36),
              .b(_36),
              .c(_93) );
        assign _37 = _93;
        nand
            the_nand_37
            ( .a(_37),
              .b(_37),
              .c(_94) );
        assign _38 = _94;
        nand
            the_nand_38
            ( .a(_38),
              .b(_35),
              .c(_95) );
        assign _39 = _95;
        nand
            the_nand_39
            ( .a(_39),
              .b(_39),
              .c(_96) );
        assign _40 = _96;
        nand
            the_nand_40
            ( .a(_40),
              .b(_20),
              .c(_97) );
        assign _41 = _97;
        nand
            the_nand_41
            ( .a(_48),
              .b(_41),
              .c(_98) );
        assign _42 = _98;
        nand
            the_nand_42
            ( .a(_99),
              .b(_45),
              .c(_100) );
        assign _43 = _100;
        assign _99 = b[2:2];
        nand
            the_nand_43
            ( .a(_101),
              .b(_99),
              .c(_102) );
        assign _45 = _102;
        assign _101 = a[2:2];
        nand
            the_nand_44
            ( .a(_101),
              .b(_45),
              .c(_103) );
        assign _47 = _103;
        nand
            the_nand_45
            ( .a(_47),
              .b(_43),
              .c(_104) );
        assign _48 = _104;
        nand
            the_nand_46
            ( .a(_48),
              .b(_42),
              .c(_105) );
        assign _49 = _105;
        nand
            the_nand_47
            ( .a(_49),
              .b(_17),
              .c(_106) );
        assign _50 = _106;
        assign _107 = { _50,
                        _16,
                        _8 };
        assign c = _107;

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

        wire [1:0] _53;
        wire _54;
        wire _1;
        wire [1:0] _55;
        wire _56;
        wire _2;
        wire [1:0] _58;
        wire _59;
        wire _3;
        wire [1:0] _61;
        wire _62;
        wire _4;
        wire [1:0] _63;
        wire _64;
        wire _5;
        wire [1:0] _65;
        wire _66;
        wire _6;
        wire [1:0] _67;
        wire _68;
        wire _7;
        wire [1:0] _69;
        wire _70;
        wire _8;
        wire [1:0] _71;
        wire _72;
        wire _9;
        wire [1:0] _73;
        wire _74;
        wire _10;
        wire [1:0] _76;
        wire _77;
        wire _11;
        wire [1:0] _79;
        wire _80;
        wire _12;
        wire [1:0] _81;
        wire _82;
        wire _13;
        wire [1:0] _83;
        wire _84;
        wire _14;
        wire [1:0] _85;
        wire _86;
        wire _15;
        wire [1:0] _87;
        wire _88;
        wire _16;
        wire [1:0] _89;
        wire _90;
        wire _17;
        wire [1:0] _91;
        wire _92;
        wire _18;
        wire [1:0] _93;
        wire _94;
        wire _19;
        wire [1:0] _95;
        wire _96;
        wire _20;
        wire [1:0] _97;
        wire _98;
        wire _21;
        wire [1:0] _99;
        wire _100;
        wire _22;
        wire [1:0] _101;
        wire _102;
        wire _23;
        wire gnd;
        wire [1:0] _103;
        wire _104;
        wire _24;
        wire [1:0] _105;
        wire _106;
        wire _25;
        wire [1:0] _107;
        wire _108;
        wire _26;
        wire _57;
        wire _60;
        wire [1:0] _109;
        wire _110;
        wire _27;
        wire [1:0] _111;
        wire _112;
        wire _28;
        wire [1:0] _113;
        wire _114;
        wire _29;
        wire [1:0] _115;
        wire _116;
        wire _30;
        wire [1:0] _117;
        wire _118;
        wire _31;
        wire [1:0] _119;
        wire _120;
        wire _32;
        wire [1:0] _121;
        wire _122;
        wire _33;
        wire [1:0] _123;
        wire _124;
        wire _34;
        wire [1:0] _125;
        wire _126;
        wire _35;
        wire _75;
        wire _78;
        wire [1:0] _127;
        wire _128;
        wire _36;
        wire [1:0] _129;
        wire _130;
        wire _37;
        wire [1:0] _131;
        wire _132;
        wire _38;
        wire [1:0] _133;
        wire _134;
        wire _39;
        wire [1:0] _135;
        wire _136;
        wire _40;
        wire [1:0] _137;
        wire _138;
        wire _41;
        wire [1:0] _139;
        wire _140;
        wire _42;
        wire [1:0] _142;
        wire _143;
        wire _43;
        wire _141;
        wire [1:0] _145;
        wire _146;
        wire _45;
        wire _144;
        wire [1:0] _147;
        wire _148;
        wire _47;
        wire [1:0] _149;
        wire _150;
        wire _48;
        wire [1:0] _151;
        wire _152;
        wire _49;
        wire [1:0] _153;
        wire _154;
        wire _50;
        wire [2:0] _155;
        assign _53 = { gnd,
                       _2 };
        LUT2
            #( .INIT("1110") )
            the_LUT2
            ( .I(_53),
              .O(_54) );
        assign _1 = _54;
        assign _55 = { _6,
                       gnd };
        LUT2
            #( .INIT("1110") )
            the_LUT2_1
            ( .I(_55),
              .O(_56) );
        assign _2 = _56;
        assign _58 = { _57,
                       _4 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_2
            ( .I(_58),
              .O(_59) );
        assign _3 = _59;
        assign _61 = { _60,
                       _57 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_3
            ( .I(_61),
              .O(_62) );
        assign _4 = _62;
        assign _63 = { _60,
                       _4 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_4
            ( .I(_63),
              .O(_64) );
        assign _5 = _64;
        assign _65 = { _5,
                       _3 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_5
            ( .I(_65),
              .O(_66) );
        assign _6 = _66;
        assign _67 = { _6,
                       _2 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_6
            ( .I(_67),
              .O(_68) );
        assign _7 = _68;
        assign _69 = { _7,
                       _1 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_7
            ( .I(_69),
              .O(_70) );
        assign _8 = _70;
        assign _71 = { _32,
                       _10 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_8
            ( .I(_71),
              .O(_72) );
        assign _9 = _72;
        assign _73 = { _14,
                       _32 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_9
            ( .I(_73),
              .O(_74) );
        assign _10 = _74;
        assign _76 = { _75,
                       _12 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_10
            ( .I(_76),
              .O(_77) );
        assign _11 = _77;
        assign _79 = { _78,
                       _75 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_11
            ( .I(_79),
              .O(_80) );
        assign _12 = _80;
        assign _81 = { _78,
                       _12 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_12
            ( .I(_81),
              .O(_82) );
        assign _13 = _82;
        assign _83 = { _13,
                       _11 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_13
            ( .I(_83),
              .O(_84) );
        assign _14 = _84;
        assign _85 = { _14,
                       _10 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_14
            ( .I(_85),
              .O(_86) );
        assign _15 = _86;
        assign _87 = { _15,
                       _9 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_15
            ( .I(_87),
              .O(_88) );
        assign _16 = _88;
        assign _89 = { _41,
                       _42 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_16
            ( .I(_89),
              .O(_90) );
        assign _17 = _90;
        assign _91 = { _32,
                       _78 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_17
            ( .I(_91),
              .O(_92) );
        assign _18 = _92;
        assign _93 = { _18,
                       _18 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_18
            ( .I(_93),
              .O(_94) );
        assign _19 = _94;
        assign _95 = { _19,
                       _19 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_19
            ( .I(_95),
              .O(_96) );
        assign _20 = _96;
        assign _97 = { gnd,
                       _60 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_20
            ( .I(_97),
              .O(_98) );
        assign _21 = _98;
        assign _99 = { _21,
                       _21 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_21
            ( .I(_99),
              .O(_100) );
        assign _22 = _100;
        assign _101 = { _22,
                        _22 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_22
            ( .I(_101),
              .O(_102) );
        assign _23 = _102;
        assign gnd = 1'b0;
        assign _103 = { _57,
                        gnd };
        LUT2
            #( .INIT("1110") )
            the_LUT2_23
            ( .I(_103),
              .O(_104) );
        assign _24 = _104;
        assign _105 = { _24,
                        _24 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_24
            ( .I(_105),
              .O(_106) );
        assign _25 = _106;
        assign _107 = { _25,
                        _25 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_25
            ( .I(_107),
              .O(_108) );
        assign _26 = _108;
        assign _57 = b[0:0];
        assign _60 = a[0:0];
        assign _109 = { _60,
                        _57 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_26
            ( .I(_109),
              .O(_110) );
        assign _27 = _110;
        assign _111 = { _27,
                        _27 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_27
            ( .I(_111),
              .O(_112) );
        assign _28 = _112;
        assign _113 = { _28,
                        _28 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_28
            ( .I(_113),
              .O(_114) );
        assign _29 = _114;
        assign _115 = { _29,
                        _26 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_29
            ( .I(_115),
              .O(_116) );
        assign _30 = _116;
        assign _117 = { _30,
                        _30 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_30
            ( .I(_117),
              .O(_118) );
        assign _31 = _118;
        assign _119 = { _31,
                        _23 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_31
            ( .I(_119),
              .O(_120) );
        assign _32 = _120;
        assign _121 = { _75,
                        _32 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_32
            ( .I(_121),
              .O(_122) );
        assign _33 = _122;
        assign _123 = { _33,
                        _33 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_33
            ( .I(_123),
              .O(_124) );
        assign _34 = _124;
        assign _125 = { _34,
                        _34 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_34
            ( .I(_125),
              .O(_126) );
        assign _35 = _126;
        assign _75 = b[1:1];
        assign _78 = a[1:1];
        assign _127 = { _78,
                        _75 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_35
            ( .I(_127),
              .O(_128) );
        assign _36 = _128;
        assign _129 = { _36,
                        _36 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_36
            ( .I(_129),
              .O(_130) );
        assign _37 = _130;
        assign _131 = { _37,
                        _37 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_37
            ( .I(_131),
              .O(_132) );
        assign _38 = _132;
        assign _133 = { _38,
                        _35 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_38
            ( .I(_133),
              .O(_134) );
        assign _39 = _134;
        assign _135 = { _39,
                        _39 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_39
            ( .I(_135),
              .O(_136) );
        assign _40 = _136;
        assign _137 = { _40,
                        _20 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_40
            ( .I(_137),
              .O(_138) );
        assign _41 = _138;
        assign _139 = { _48,
                        _41 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_41
            ( .I(_139),
              .O(_140) );
        assign _42 = _140;
        assign _142 = { _141,
                        _45 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_42
            ( .I(_142),
              .O(_143) );
        assign _43 = _143;
        assign _141 = b[2:2];
        assign _145 = { _144,
                        _141 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_43
            ( .I(_145),
              .O(_146) );
        assign _45 = _146;
        assign _144 = a[2:2];
        assign _147 = { _144,
                        _45 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_44
            ( .I(_147),
              .O(_148) );
        assign _47 = _148;
        assign _149 = { _47,
                        _43 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_45
            ( .I(_149),
              .O(_150) );
        assign _48 = _150;
        assign _151 = { _48,
                        _42 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_46
            ( .I(_151),
              .O(_152) );
        assign _49 = _152;
        assign _153 = { _49,
                        _17 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_47
            ( .I(_153),
              .O(_154) );
        assign _50 = _154;
        assign _155 = { _50,
                        _16,
                        _8 };
        assign c = _155;

    endmodule
    |}]
;;
