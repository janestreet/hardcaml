(* Build adder circuits using NAND as the base gate.  Demonstrates the use of the
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
            (List.init 8 ~f:(fun b -> Bits_nand.(of_int ~width:3 a +: of_int ~width:3 b))
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
    Map.find_exn
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

        wire _54;
        wire _1;
        wire _56;
        wire _2;
        wire _59;
        wire _3;
        wire _62;
        wire _4;
        wire _64;
        wire _5;
        wire _66;
        wire _6;
        wire _68;
        wire _7;
        wire _70;
        wire _8;
        wire _72;
        wire _9;
        wire _74;
        wire _10;
        wire _77;
        wire _11;
        wire _80;
        wire _12;
        wire _82;
        wire _13;
        wire _84;
        wire _14;
        wire _86;
        wire _15;
        wire _88;
        wire _16;
        wire _90;
        wire _17;
        wire _92;
        wire _18;
        wire _94;
        wire _19;
        wire _96;
        wire _20;
        wire _98;
        wire _21;
        wire _100;
        wire _22;
        wire _102;
        wire _23;
        wire _52;
        wire _104;
        wire _24;
        wire _106;
        wire _25;
        wire _108;
        wire _26;
        wire _57;
        wire _60;
        wire _110;
        wire _27;
        wire _112;
        wire _28;
        wire _114;
        wire _29;
        wire _116;
        wire _30;
        wire _118;
        wire _31;
        wire _120;
        wire _32;
        wire _122;
        wire _33;
        wire _124;
        wire _34;
        wire _126;
        wire _35;
        wire _75;
        wire _78;
        wire _128;
        wire _36;
        wire _130;
        wire _37;
        wire _132;
        wire _38;
        wire _134;
        wire _39;
        wire _136;
        wire _40;
        wire _138;
        wire _41;
        wire _140;
        wire _42;
        wire _143;
        wire _43;
        wire _141;
        wire _146;
        wire _45;
        wire _144;
        wire _148;
        wire _47;
        wire _150;
        wire _48;
        wire _152;
        wire _49;
        wire _154;
        wire _50;
        wire [2:0] _155;
        nand
            the_nand
            ( .a(_52),
              .b(_2),
              .c(_54) );
        assign _1 = _54;
        nand
            the_nand_0
            ( .a(_6),
              .b(_52),
              .c(_56) );
        assign _2 = _56;
        nand
            the_nand_1
            ( .a(_57),
              .b(_4),
              .c(_59) );
        assign _3 = _59;
        nand
            the_nand_2
            ( .a(_60),
              .b(_57),
              .c(_62) );
        assign _4 = _62;
        nand
            the_nand_3
            ( .a(_60),
              .b(_4),
              .c(_64) );
        assign _5 = _64;
        nand
            the_nand_4
            ( .a(_5),
              .b(_3),
              .c(_66) );
        assign _6 = _66;
        nand
            the_nand_5
            ( .a(_6),
              .b(_2),
              .c(_68) );
        assign _7 = _68;
        nand
            the_nand_6
            ( .a(_7),
              .b(_1),
              .c(_70) );
        assign _8 = _70;
        nand
            the_nand_7
            ( .a(_32),
              .b(_10),
              .c(_72) );
        assign _9 = _72;
        nand
            the_nand_8
            ( .a(_14),
              .b(_32),
              .c(_74) );
        assign _10 = _74;
        nand
            the_nand_9
            ( .a(_75),
              .b(_12),
              .c(_77) );
        assign _11 = _77;
        nand
            the_nand_10
            ( .a(_78),
              .b(_75),
              .c(_80) );
        assign _12 = _80;
        nand
            the_nand_11
            ( .a(_78),
              .b(_12),
              .c(_82) );
        assign _13 = _82;
        nand
            the_nand_12
            ( .a(_13),
              .b(_11),
              .c(_84) );
        assign _14 = _84;
        nand
            the_nand_13
            ( .a(_14),
              .b(_10),
              .c(_86) );
        assign _15 = _86;
        nand
            the_nand_14
            ( .a(_15),
              .b(_9),
              .c(_88) );
        assign _16 = _88;
        nand
            the_nand_15
            ( .a(_41),
              .b(_42),
              .c(_90) );
        assign _17 = _90;
        nand
            the_nand_16
            ( .a(_32),
              .b(_78),
              .c(_92) );
        assign _18 = _92;
        nand
            the_nand_17
            ( .a(_18),
              .b(_18),
              .c(_94) );
        assign _19 = _94;
        nand
            the_nand_18
            ( .a(_19),
              .b(_19),
              .c(_96) );
        assign _20 = _96;
        nand
            the_nand_19
            ( .a(_52),
              .b(_60),
              .c(_98) );
        assign _21 = _98;
        nand
            the_nand_20
            ( .a(_21),
              .b(_21),
              .c(_100) );
        assign _22 = _100;
        nand
            the_nand_21
            ( .a(_22),
              .b(_22),
              .c(_102) );
        assign _23 = _102;
        assign _52 = 1'b0;
        nand
            the_nand_22
            ( .a(_57),
              .b(_52),
              .c(_104) );
        assign _24 = _104;
        nand
            the_nand_23
            ( .a(_24),
              .b(_24),
              .c(_106) );
        assign _25 = _106;
        nand
            the_nand_24
            ( .a(_25),
              .b(_25),
              .c(_108) );
        assign _26 = _108;
        assign _57 = b[0:0];
        assign _60 = a[0:0];
        nand
            the_nand_25
            ( .a(_60),
              .b(_57),
              .c(_110) );
        assign _27 = _110;
        nand
            the_nand_26
            ( .a(_27),
              .b(_27),
              .c(_112) );
        assign _28 = _112;
        nand
            the_nand_27
            ( .a(_28),
              .b(_28),
              .c(_114) );
        assign _29 = _114;
        nand
            the_nand_28
            ( .a(_29),
              .b(_26),
              .c(_116) );
        assign _30 = _116;
        nand
            the_nand_29
            ( .a(_30),
              .b(_30),
              .c(_118) );
        assign _31 = _118;
        nand
            the_nand_30
            ( .a(_31),
              .b(_23),
              .c(_120) );
        assign _32 = _120;
        nand
            the_nand_31
            ( .a(_75),
              .b(_32),
              .c(_122) );
        assign _33 = _122;
        nand
            the_nand_32
            ( .a(_33),
              .b(_33),
              .c(_124) );
        assign _34 = _124;
        nand
            the_nand_33
            ( .a(_34),
              .b(_34),
              .c(_126) );
        assign _35 = _126;
        assign _75 = b[1:1];
        assign _78 = a[1:1];
        nand
            the_nand_34
            ( .a(_78),
              .b(_75),
              .c(_128) );
        assign _36 = _128;
        nand
            the_nand_35
            ( .a(_36),
              .b(_36),
              .c(_130) );
        assign _37 = _130;
        nand
            the_nand_36
            ( .a(_37),
              .b(_37),
              .c(_132) );
        assign _38 = _132;
        nand
            the_nand_37
            ( .a(_38),
              .b(_35),
              .c(_134) );
        assign _39 = _134;
        nand
            the_nand_38
            ( .a(_39),
              .b(_39),
              .c(_136) );
        assign _40 = _136;
        nand
            the_nand_39
            ( .a(_40),
              .b(_20),
              .c(_138) );
        assign _41 = _138;
        nand
            the_nand_40
            ( .a(_48),
              .b(_41),
              .c(_140) );
        assign _42 = _140;
        nand
            the_nand_41
            ( .a(_141),
              .b(_45),
              .c(_143) );
        assign _43 = _143;
        assign _141 = b[2:2];
        nand
            the_nand_42
            ( .a(_144),
              .b(_141),
              .c(_146) );
        assign _45 = _146;
        assign _144 = a[2:2];
        nand
            the_nand_43
            ( .a(_144),
              .b(_45),
              .c(_148) );
        assign _47 = _148;
        nand
            the_nand_44
            ( .a(_47),
              .b(_43),
              .c(_150) );
        assign _48 = _150;
        nand
            the_nand_45
            ( .a(_48),
              .b(_42),
              .c(_152) );
        assign _49 = _152;
        nand
            the_nand_46
            ( .a(_49),
              .b(_17),
              .c(_154) );
        assign _50 = _154;
        assign _155 = { _50,
                        _16,
                        _8 };
        assign c = _155;

    endmodule
    |}]
;;

module Fpga_nand = Comb.Make (Comb.Make_primitives (Make_nand_gates (struct
  module Bits = Signal
  open Signal

  let nand_lut a b =
    Map.find_exn
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
        wire _55;
        wire _1;
        wire [1:0] _56;
        wire _58;
        wire _2;
        wire [1:0] _60;
        wire _62;
        wire _3;
        wire [1:0] _64;
        wire _66;
        wire _4;
        wire [1:0] _67;
        wire _69;
        wire _5;
        wire [1:0] _70;
        wire _72;
        wire _6;
        wire [1:0] _73;
        wire _75;
        wire _7;
        wire [1:0] _76;
        wire _78;
        wire _8;
        wire [1:0] _79;
        wire _81;
        wire _9;
        wire [1:0] _82;
        wire _84;
        wire _10;
        wire [1:0] _86;
        wire _88;
        wire _11;
        wire [1:0] _90;
        wire _92;
        wire _12;
        wire [1:0] _93;
        wire _95;
        wire _13;
        wire [1:0] _96;
        wire _98;
        wire _14;
        wire [1:0] _99;
        wire _101;
        wire _15;
        wire [1:0] _102;
        wire _104;
        wire _16;
        wire [1:0] _105;
        wire _107;
        wire _17;
        wire [1:0] _108;
        wire _110;
        wire _18;
        wire [1:0] _111;
        wire _113;
        wire _19;
        wire [1:0] _114;
        wire _116;
        wire _20;
        wire [1:0] _117;
        wire _119;
        wire _21;
        wire [1:0] _120;
        wire _122;
        wire _22;
        wire [1:0] _123;
        wire _125;
        wire _23;
        wire _52;
        wire [1:0] _126;
        wire _128;
        wire _24;
        wire [1:0] _129;
        wire _131;
        wire _25;
        wire [1:0] _132;
        wire _134;
        wire _26;
        wire _59;
        wire _63;
        wire [1:0] _135;
        wire _137;
        wire _27;
        wire [1:0] _138;
        wire _140;
        wire _28;
        wire [1:0] _141;
        wire _143;
        wire _29;
        wire [1:0] _144;
        wire _146;
        wire _30;
        wire [1:0] _147;
        wire _149;
        wire _31;
        wire [1:0] _150;
        wire _152;
        wire _32;
        wire [1:0] _153;
        wire _155;
        wire _33;
        wire [1:0] _156;
        wire _158;
        wire _34;
        wire [1:0] _159;
        wire _161;
        wire _35;
        wire _85;
        wire _89;
        wire [1:0] _162;
        wire _164;
        wire _36;
        wire [1:0] _165;
        wire _167;
        wire _37;
        wire [1:0] _168;
        wire _170;
        wire _38;
        wire [1:0] _171;
        wire _173;
        wire _39;
        wire [1:0] _174;
        wire _176;
        wire _40;
        wire [1:0] _177;
        wire _179;
        wire _41;
        wire [1:0] _180;
        wire _182;
        wire _42;
        wire [1:0] _184;
        wire _186;
        wire _43;
        wire _183;
        wire [1:0] _188;
        wire _190;
        wire _45;
        wire _187;
        wire [1:0] _191;
        wire _193;
        wire _47;
        wire [1:0] _194;
        wire _196;
        wire _48;
        wire [1:0] _197;
        wire _199;
        wire _49;
        wire [1:0] _200;
        wire _202;
        wire _50;
        wire [2:0] _203;
        assign _53 = { _52,
                       _2 };
        LUT2
            #( .INIT("1110") )
            the_LUT2
            ( .I(_53),
              .O(_55) );
        assign _1 = _55;
        assign _56 = { _6,
                       _52 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_0
            ( .I(_56),
              .O(_58) );
        assign _2 = _58;
        assign _60 = { _59,
                       _4 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_1
            ( .I(_60),
              .O(_62) );
        assign _3 = _62;
        assign _64 = { _63,
                       _59 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_2
            ( .I(_64),
              .O(_66) );
        assign _4 = _66;
        assign _67 = { _63,
                       _4 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_3
            ( .I(_67),
              .O(_69) );
        assign _5 = _69;
        assign _70 = { _5,
                       _3 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_4
            ( .I(_70),
              .O(_72) );
        assign _6 = _72;
        assign _73 = { _6,
                       _2 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_5
            ( .I(_73),
              .O(_75) );
        assign _7 = _75;
        assign _76 = { _7,
                       _1 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_6
            ( .I(_76),
              .O(_78) );
        assign _8 = _78;
        assign _79 = { _32,
                       _10 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_7
            ( .I(_79),
              .O(_81) );
        assign _9 = _81;
        assign _82 = { _14,
                       _32 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_8
            ( .I(_82),
              .O(_84) );
        assign _10 = _84;
        assign _86 = { _85,
                       _12 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_9
            ( .I(_86),
              .O(_88) );
        assign _11 = _88;
        assign _90 = { _89,
                       _85 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_10
            ( .I(_90),
              .O(_92) );
        assign _12 = _92;
        assign _93 = { _89,
                       _12 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_11
            ( .I(_93),
              .O(_95) );
        assign _13 = _95;
        assign _96 = { _13,
                       _11 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_12
            ( .I(_96),
              .O(_98) );
        assign _14 = _98;
        assign _99 = { _14,
                       _10 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_13
            ( .I(_99),
              .O(_101) );
        assign _15 = _101;
        assign _102 = { _15,
                        _9 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_14
            ( .I(_102),
              .O(_104) );
        assign _16 = _104;
        assign _105 = { _41,
                        _42 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_15
            ( .I(_105),
              .O(_107) );
        assign _17 = _107;
        assign _108 = { _32,
                        _89 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_16
            ( .I(_108),
              .O(_110) );
        assign _18 = _110;
        assign _111 = { _18,
                        _18 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_17
            ( .I(_111),
              .O(_113) );
        assign _19 = _113;
        assign _114 = { _19,
                        _19 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_18
            ( .I(_114),
              .O(_116) );
        assign _20 = _116;
        assign _117 = { _52,
                        _63 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_19
            ( .I(_117),
              .O(_119) );
        assign _21 = _119;
        assign _120 = { _21,
                        _21 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_20
            ( .I(_120),
              .O(_122) );
        assign _22 = _122;
        assign _123 = { _22,
                        _22 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_21
            ( .I(_123),
              .O(_125) );
        assign _23 = _125;
        assign _52 = 1'b0;
        assign _126 = { _59,
                        _52 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_22
            ( .I(_126),
              .O(_128) );
        assign _24 = _128;
        assign _129 = { _24,
                        _24 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_23
            ( .I(_129),
              .O(_131) );
        assign _25 = _131;
        assign _132 = { _25,
                        _25 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_24
            ( .I(_132),
              .O(_134) );
        assign _26 = _134;
        assign _59 = b[0:0];
        assign _63 = a[0:0];
        assign _135 = { _63,
                        _59 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_25
            ( .I(_135),
              .O(_137) );
        assign _27 = _137;
        assign _138 = { _27,
                        _27 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_26
            ( .I(_138),
              .O(_140) );
        assign _28 = _140;
        assign _141 = { _28,
                        _28 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_27
            ( .I(_141),
              .O(_143) );
        assign _29 = _143;
        assign _144 = { _29,
                        _26 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_28
            ( .I(_144),
              .O(_146) );
        assign _30 = _146;
        assign _147 = { _30,
                        _30 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_29
            ( .I(_147),
              .O(_149) );
        assign _31 = _149;
        assign _150 = { _31,
                        _23 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_30
            ( .I(_150),
              .O(_152) );
        assign _32 = _152;
        assign _153 = { _85,
                        _32 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_31
            ( .I(_153),
              .O(_155) );
        assign _33 = _155;
        assign _156 = { _33,
                        _33 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_32
            ( .I(_156),
              .O(_158) );
        assign _34 = _158;
        assign _159 = { _34,
                        _34 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_33
            ( .I(_159),
              .O(_161) );
        assign _35 = _161;
        assign _85 = b[1:1];
        assign _89 = a[1:1];
        assign _162 = { _89,
                        _85 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_34
            ( .I(_162),
              .O(_164) );
        assign _36 = _164;
        assign _165 = { _36,
                        _36 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_35
            ( .I(_165),
              .O(_167) );
        assign _37 = _167;
        assign _168 = { _37,
                        _37 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_36
            ( .I(_168),
              .O(_170) );
        assign _38 = _170;
        assign _171 = { _38,
                        _35 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_37
            ( .I(_171),
              .O(_173) );
        assign _39 = _173;
        assign _174 = { _39,
                        _39 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_38
            ( .I(_174),
              .O(_176) );
        assign _40 = _176;
        assign _177 = { _40,
                        _20 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_39
            ( .I(_177),
              .O(_179) );
        assign _41 = _179;
        assign _180 = { _48,
                        _41 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_40
            ( .I(_180),
              .O(_182) );
        assign _42 = _182;
        assign _184 = { _183,
                        _45 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_41
            ( .I(_184),
              .O(_186) );
        assign _43 = _186;
        assign _183 = b[2:2];
        assign _188 = { _187,
                        _183 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_42
            ( .I(_188),
              .O(_190) );
        assign _45 = _190;
        assign _187 = a[2:2];
        assign _191 = { _187,
                        _45 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_43
            ( .I(_191),
              .O(_193) );
        assign _47 = _193;
        assign _194 = { _47,
                        _43 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_44
            ( .I(_194),
              .O(_196) );
        assign _48 = _196;
        assign _197 = { _48,
                        _42 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_45
            ( .I(_197),
              .O(_199) );
        assign _49 = _199;
        assign _200 = { _49,
                        _17 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_46
            ( .I(_200),
              .O(_202) );
        assign _50 = _202;
        assign _203 = { _50,
                        _16,
                        _8 };
        assign c = _203;

    endmodule
    |}]
;;
