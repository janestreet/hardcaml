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
    (111 000 001 010 011 100 101 110) |}]
;;

module Asic_nand = Comb.Make (Comb.Make_primitives (Make_nand_gates (struct
                                                      module Bits = Signal

                                                      let nand a b =
                                                        assert (Bits.width a = Bits.width b);
                                                        (Instantiation.create
                                                           ()
                                                           ~name:"nand"
                                                           ~inputs:[ "a", a; "b", b ]
                                                           ~outputs:[ "c", Bits.width a ])
                                                        #o
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

        /* signal declarations */
        wire _104;
        wire _100;
        wire _96;
        wire _92;
        wire _94;
        wire _98;
        wire _102;
        wire _106;
        wire _88;
        wire _84;
        wire _80;
        wire _76;
        wire _78;
        wire _82;
        wire _86;
        wire _90;
        wire _72;
        wire _60;
        wire _62;
        wire _64;
        wire _42;
        wire _44;
        wire _46;
        wire _30 = 1'b0;
        wire _32;
        wire _34;
        wire _36;
        wire _23;
        wire _22;
        wire _25;
        wire _27;
        wire _29;
        wire _38;
        wire _40;
        wire _48;
        wire _50;
        wire _52;
        wire _54;
        wire _15;
        wire _14;
        wire _17;
        wire _19;
        wire _21;
        wire _56;
        wire _58;
        wire _66;
        wire _68;
        wire _11;
        wire _5;
        wire _7;
        wire _4;
        wire _9;
        wire _13;
        wire _70;
        wire _74;
        wire [2:0] _107;

        /* logic */
        nand
            the_nand
            ( .a(_30), .b(_100), .c(_104) );
        nand
            the_nand_0
            ( .a(_98), .b(_30), .c(_100) );
        nand
            the_nand_1
            ( .a(_23), .b(_92), .c(_96) );
        nand
            the_nand_2
            ( .a(_22), .b(_23), .c(_92) );
        nand
            the_nand_3
            ( .a(_22), .b(_92), .c(_94) );
        nand
            the_nand_4
            ( .a(_94), .b(_96), .c(_98) );
        nand
            the_nand_5
            ( .a(_98), .b(_100), .c(_102) );
        nand
            the_nand_6
            ( .a(_102), .b(_104), .c(_106) );
        nand
            the_nand_7
            ( .a(_48), .b(_84), .c(_88) );
        nand
            the_nand_8
            ( .a(_82), .b(_48), .c(_84) );
        nand
            the_nand_9
            ( .a(_15), .b(_76), .c(_80) );
        nand
            the_nand_10
            ( .a(_14), .b(_15), .c(_76) );
        nand
            the_nand_11
            ( .a(_14), .b(_76), .c(_78) );
        nand
            the_nand_12
            ( .a(_78), .b(_80), .c(_82) );
        nand
            the_nand_13
            ( .a(_82), .b(_84), .c(_86) );
        nand
            the_nand_14
            ( .a(_86), .b(_88), .c(_90) );
        nand
            the_nand_15
            ( .a(_66), .b(_68), .c(_72) );
        nand
            the_nand_16
            ( .a(_48), .b(_14), .c(_60) );
        nand
            the_nand_17
            ( .a(_60), .b(_60), .c(_62) );
        nand
            the_nand_18
            ( .a(_62), .b(_62), .c(_64) );
        nand
            the_nand_19
            ( .a(_30), .b(_22), .c(_42) );
        nand
            the_nand_20
            ( .a(_42), .b(_42), .c(_44) );
        nand
            the_nand_21
            ( .a(_44), .b(_44), .c(_46) );
        nand
            the_nand_22
            ( .a(_23), .b(_30), .c(_32) );
        nand
            the_nand_23
            ( .a(_32), .b(_32), .c(_34) );
        nand
            the_nand_24
            ( .a(_34), .b(_34), .c(_36) );
        assign _23 = b[0:0];
        assign _22 = a[0:0];
        nand
            the_nand_25
            ( .a(_22), .b(_23), .c(_25) );
        nand
            the_nand_26
            ( .a(_25), .b(_25), .c(_27) );
        nand
            the_nand_27
            ( .a(_27), .b(_27), .c(_29) );
        nand
            the_nand_28
            ( .a(_29), .b(_36), .c(_38) );
        nand
            the_nand_29
            ( .a(_38), .b(_38), .c(_40) );
        nand
            the_nand_30
            ( .a(_40), .b(_46), .c(_48) );
        nand
            the_nand_31
            ( .a(_15), .b(_48), .c(_50) );
        nand
            the_nand_32
            ( .a(_50), .b(_50), .c(_52) );
        nand
            the_nand_33
            ( .a(_52), .b(_52), .c(_54) );
        assign _15 = b[1:1];
        assign _14 = a[1:1];
        nand
            the_nand_34
            ( .a(_14), .b(_15), .c(_17) );
        nand
            the_nand_35
            ( .a(_17), .b(_17), .c(_19) );
        nand
            the_nand_36
            ( .a(_19), .b(_19), .c(_21) );
        nand
            the_nand_37
            ( .a(_21), .b(_54), .c(_56) );
        nand
            the_nand_38
            ( .a(_56), .b(_56), .c(_58) );
        nand
            the_nand_39
            ( .a(_58), .b(_64), .c(_66) );
        nand
            the_nand_40
            ( .a(_13), .b(_66), .c(_68) );
        nand
            the_nand_41
            ( .a(_5), .b(_7), .c(_11) );
        assign _5 = b[2:2];
        nand
            the_nand_42
            ( .a(_4), .b(_5), .c(_7) );
        assign _4 = a[2:2];
        nand
            the_nand_43
            ( .a(_4), .b(_7), .c(_9) );
        nand
            the_nand_44
            ( .a(_9), .b(_11), .c(_13) );
        nand
            the_nand_45
            ( .a(_13), .b(_68), .c(_70) );
        nand
            the_nand_46
            ( .a(_70), .b(_72), .c(_74) );
        assign _107 = { _74, _90, _106 };

        /* aliases */

        /* output assignments */
        assign c = _107;

    endmodule |}]
;;

module Fpga_nand = Comb.Make (Comb.Make_primitives (Make_nand_gates (struct
                                                      module Bits = Signal
                                                      open Signal

                                                      let nand_lut a b =
                                                        (Instantiation.create
                                                           ()
                                                           ~name:"LUT2"
                                                           ~parameters:[ Parameter.create ~name:"INIT" ~value:(String "1110") ]
                                                           ~inputs:[ "I", a @: b ]
                                                           ~outputs:[ "O", 1 ])
                                                        #o
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

        /* signal declarations */
        wire [1:0] _149;
        wire _151;
        wire [1:0] _143;
        wire _145;
        wire [1:0] _137;
        wire _139;
        wire [1:0] _131;
        wire _133;
        wire [1:0] _134;
        wire _136;
        wire [1:0] _140;
        wire _142;
        wire [1:0] _146;
        wire _148;
        wire [1:0] _152;
        wire _154;
        wire [1:0] _125;
        wire _127;
        wire [1:0] _119;
        wire _121;
        wire [1:0] _113;
        wire _115;
        wire [1:0] _107;
        wire _109;
        wire [1:0] _110;
        wire _112;
        wire [1:0] _116;
        wire _118;
        wire [1:0] _122;
        wire _124;
        wire [1:0] _128;
        wire _130;
        wire [1:0] _101;
        wire _103;
        wire [1:0] _83;
        wire _85;
        wire [1:0] _86;
        wire _88;
        wire [1:0] _89;
        wire _91;
        wire [1:0] _56;
        wire _58;
        wire [1:0] _59;
        wire _61;
        wire [1:0] _62;
        wire _64;
        wire _40 = 1'b0;
        wire [1:0] _41;
        wire _43;
        wire [1:0] _44;
        wire _46;
        wire [1:0] _47;
        wire _49;
        wire _30;
        wire _29;
        wire [1:0] _31;
        wire _33;
        wire [1:0] _34;
        wire _36;
        wire [1:0] _37;
        wire _39;
        wire [1:0] _50;
        wire _52;
        wire [1:0] _53;
        wire _55;
        wire [1:0] _65;
        wire _67;
        wire [1:0] _68;
        wire _70;
        wire [1:0] _71;
        wire _73;
        wire [1:0] _74;
        wire _76;
        wire _19;
        wire _18;
        wire [1:0] _20;
        wire _22;
        wire [1:0] _23;
        wire _25;
        wire [1:0] _26;
        wire _28;
        wire [1:0] _77;
        wire _79;
        wire [1:0] _80;
        wire _82;
        wire [1:0] _92;
        wire _94;
        wire [1:0] _95;
        wire _97;
        wire [1:0] _12;
        wire _14;
        wire _5;
        wire [1:0] _6;
        wire _8;
        wire _4;
        wire [1:0] _9;
        wire _11;
        wire [1:0] _15;
        wire _17;
        wire [1:0] _98;
        wire _100;
        wire [1:0] _104;
        wire _106;
        wire [2:0] _155;

        /* logic */
        assign _149 = { _40, _145 };
        LUT2
            #( .INIT("1110") )
            the_LUT2
            ( .I(_149), .O(_151) );
        assign _143 = { _142, _40 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_0
            ( .I(_143), .O(_145) );
        assign _137 = { _30, _133 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_1
            ( .I(_137), .O(_139) );
        assign _131 = { _29, _30 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_2
            ( .I(_131), .O(_133) );
        assign _134 = { _29, _133 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_3
            ( .I(_134), .O(_136) );
        assign _140 = { _136, _139 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_4
            ( .I(_140), .O(_142) );
        assign _146 = { _142, _145 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_5
            ( .I(_146), .O(_148) );
        assign _152 = { _148, _151 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_6
            ( .I(_152), .O(_154) );
        assign _125 = { _67, _121 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_7
            ( .I(_125), .O(_127) );
        assign _119 = { _118, _67 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_8
            ( .I(_119), .O(_121) );
        assign _113 = { _19, _109 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_9
            ( .I(_113), .O(_115) );
        assign _107 = { _18, _19 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_10
            ( .I(_107), .O(_109) );
        assign _110 = { _18, _109 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_11
            ( .I(_110), .O(_112) );
        assign _116 = { _112, _115 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_12
            ( .I(_116), .O(_118) );
        assign _122 = { _118, _121 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_13
            ( .I(_122), .O(_124) );
        assign _128 = { _124, _127 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_14
            ( .I(_128), .O(_130) );
        assign _101 = { _94, _97 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_15
            ( .I(_101), .O(_103) );
        assign _83 = { _67, _18 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_16
            ( .I(_83), .O(_85) );
        assign _86 = { _85, _85 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_17
            ( .I(_86), .O(_88) );
        assign _89 = { _88, _88 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_18
            ( .I(_89), .O(_91) );
        assign _56 = { _40, _29 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_19
            ( .I(_56), .O(_58) );
        assign _59 = { _58, _58 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_20
            ( .I(_59), .O(_61) );
        assign _62 = { _61, _61 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_21
            ( .I(_62), .O(_64) );
        assign _41 = { _30, _40 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_22
            ( .I(_41), .O(_43) );
        assign _44 = { _43, _43 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_23
            ( .I(_44), .O(_46) );
        assign _47 = { _46, _46 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_24
            ( .I(_47), .O(_49) );
        assign _30 = b[0:0];
        assign _29 = a[0:0];
        assign _31 = { _29, _30 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_25
            ( .I(_31), .O(_33) );
        assign _34 = { _33, _33 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_26
            ( .I(_34), .O(_36) );
        assign _37 = { _36, _36 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_27
            ( .I(_37), .O(_39) );
        assign _50 = { _39, _49 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_28
            ( .I(_50), .O(_52) );
        assign _53 = { _52, _52 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_29
            ( .I(_53), .O(_55) );
        assign _65 = { _55, _64 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_30
            ( .I(_65), .O(_67) );
        assign _68 = { _19, _67 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_31
            ( .I(_68), .O(_70) );
        assign _71 = { _70, _70 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_32
            ( .I(_71), .O(_73) );
        assign _74 = { _73, _73 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_33
            ( .I(_74), .O(_76) );
        assign _19 = b[1:1];
        assign _18 = a[1:1];
        assign _20 = { _18, _19 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_34
            ( .I(_20), .O(_22) );
        assign _23 = { _22, _22 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_35
            ( .I(_23), .O(_25) );
        assign _26 = { _25, _25 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_36
            ( .I(_26), .O(_28) );
        assign _77 = { _28, _76 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_37
            ( .I(_77), .O(_79) );
        assign _80 = { _79, _79 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_38
            ( .I(_80), .O(_82) );
        assign _92 = { _82, _91 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_39
            ( .I(_92), .O(_94) );
        assign _95 = { _17, _94 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_40
            ( .I(_95), .O(_97) );
        assign _12 = { _5, _8 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_41
            ( .I(_12), .O(_14) );
        assign _5 = b[2:2];
        assign _6 = { _4, _5 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_42
            ( .I(_6), .O(_8) );
        assign _4 = a[2:2];
        assign _9 = { _4, _8 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_43
            ( .I(_9), .O(_11) );
        assign _15 = { _11, _14 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_44
            ( .I(_15), .O(_17) );
        assign _98 = { _17, _97 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_45
            ( .I(_98), .O(_100) );
        assign _104 = { _100, _103 };
        LUT2
            #( .INIT("1110") )
            the_LUT2_46
            ( .I(_104), .O(_106) );
        assign _155 = { _106, _130, _154 };

        /* aliases */

        /* output assignments */
        assign c = _155;

    endmodule |}]
;;
