open! Import

module I = struct
  type 'a t = { a : 'a [@bits 2] } [@@deriving hardcaml]
end

module O = struct
  type 'a t = { b : 'a [@bits 3] } [@@deriving hardcaml]
end

module T = struct
  type 'a t = { c : 'a } [@@deriving hardcaml]
end

let print_rope r = Rope.to_string r |> Stdio.print_string

let%expect_test "Prints with circuit created via With_interface.create_circuit" =
  let open Structural.With_interface (I) (O) (T) in
  (* Simple example with attributes *)
  Structural.to_verilog
    (create_circuit "foo" (fun i o t ->
       Structural.add_attribute i.a (Rtl_attribute.Vivado.io_buffer_type `IBUF);
       Structural.add_attribute o.b (Rtl_attribute.Vivado.io_buffer_type `OBUF);
       Structural.add_attribute t.c (Rtl_attribute.Vivado.io_buffer_type `None);
       ()))
  |> print_rope;
  [%expect
    {|
    module foo
    (
      (* io_buffer_type="IBUF" *)
      input [1:0] a,
      (* io_buffer_type="OBUF" *)
      output [2:0] b,
      (* io_buffer_type="none" *)
      inout c
    );

    endmodule
    |}];
  (* An example that does some simple assignments *)
  Structural.to_verilog
    (create_circuit "bar" (fun i o t ->
       let open Structural in
       t.c <-- mux (select i.a ~high:0 ~low:0) [ z 1; select i.a ~high:1 ~low:1 ];
       o.b <-- concat_msb [ select i.a ~high:0 ~low:0; i.a ]))
  |> print_rope;
  [%expect
    {|
    module bar
    (
      input [1:0] a,
      output [2:0] b,
      inout c
    );

      wire _4;
      wire _5;
      wire _6;
      wire _7;
      wire _8;
      wire [2:0] _9;
      assign _4 = a[1:1];
      assign _5 = 1'bz;
      assign _6 = a[0:0];
      assign _7 =
        _6 == 0 ? _5 :
        _4;
      assign _8 = a[0:0];
      assign _9 = { _8, a };
      assign b = _9;
      assign c = _7;
    endmodule
    |}];
  (* An instantiation example *)
  Structural.to_verilog (create_circuit "baz" (fun i o t -> inst "inner_baz" i o t))
  |> print_rope;
  [%expect
    {|
    module baz
    (
      input [1:0] a,
      output [2:0] b,
      inout c
    );

      inner_baz _4
      (
        .a(a),
        .b(b),
        .c(c)
      );
    endmodule
    |}]
;;

module Structural_sexp_of_test = struct
  let test name =
    Structural.start_circuit name;
    let module B = Structural.Lib () in
    let open Structural in
    let open B in
    let a = mk_input "a" 8 in
    let b = mk_input "b" 8 in
    let const_o = mk_output "const" 8 in
    let sum_o = mk_output "sum" 8 in
    let cat_o = mk_output "cat" 16 in
    let sel_o = mk_output "sel" 4 in
    let mux_o = mk_output "mux" 8 in
    let const = of_int_trunc ~width:8 123 in
    let sum = a +: b in
    let cat = a @: b in
    let sel = select a ~high:3 ~low:0 in
    let mux = mux (select a ~high:1 ~low:0) [ a; b; const ] in
    let out_wire = mk_wire 5 in
    let out_direct = mk_output "out_direct" 5 in
    let out_indirect = mk_output "out_indirect" 5 in
    let tri_wire = mk_triwire 2 in
    let tri_direct = mk_tristate "tri_direct" 2 in
    let tri_indirect = mk_tristate "tri_indirect" 2 in
    inst
      "foo"
      ~g:[ "G", GInt 7 ]
      ~i:[ "I" ==> mux ]
      ~o:[ "O" ==> out_wire; "OO" ==> out_direct ]
      ~t:[ "T" ==> tri_wire; "TO" ==> tri_direct ];
    tri_indirect <-- tri_wire;
    out_indirect <-- out_wire;
    const_o <-- const;
    sum_o <-- sum;
    cat_o <-- cat;
    sel_o <-- sel;
    mux_o <-- mux;
    end_circuit ();
    print_s (sexp_of_t a);
    print_s (sexp_of_t const);
    print_s (sexp_of_t cat);
    print_s (sexp_of_t sel);
    print_s (sexp_of_t mux);
    print_s (sexp_of_t sum);
    print_s (sexp_of_t sum_o);
    print_s (sexp_of_t tri_wire);
    print_s (sexp_of_t tri_direct);
    print_s (sexp_of_t tri_indirect);
    print_s (sexp_of_t out_wire);
    print_s (sexp_of_t out_direct);
    print_s (sexp_of_t out_indirect)
  ;;
end

let%expect_test "constant inbetween circuits" =
  let open Structural in
  let _ : circuit =
    create_circuit "bad_const" (fun () ->
      let _ : signal = of_bit_string "1" in
      ())
  in
  require_does_raise (fun () -> of_bit_string "0");
  [%expect {| (Hardcaml__Structural.No_circuit) |}];
  require_does_raise (fun () -> of_bit_string "1");
  [%expect {| (Hardcaml__Structural.No_circuit) |}]
;;

let%expect_test "Structural.Base0 sexp_of_t" =
  let module T = Structural_sexp_of_test in
  T.test "base0";
  [%expect
    {|
    (Module_input
      (id    3)
      (name  a)
      (width 8))
    (Constant
      (id    10)
      (width 8)
      (value 01111011))
    (Concat
      (id    13)
      (width 16))
    (Select
      (id    14)
      (width 4)
      (range (3 0)))
    (Mux
      (id    16)
      (width 8))
    (Internal_wire
      (id    11)
      (width 8))
    (Module_output
      (id    6)
      (name  sum)
      (width 8))
    (Internal_triwire
      (id    20)
      (width 2))
    (Module_tristate
      (id    21)
      (name  tri_direct)
      (width 2))
    (Module_tristate
      (id    22)
      (name  tri_indirect)
      (width 2))
    (Internal_wire
      (id    17)
      (width 5))
    (Module_output
      (id    18)
      (name  out_direct)
      (width 5))
    (Module_output
      (id    19)
      (name  out_indirect)
      (width 5))
    |}]
;;

let%expect_test "structural rtl comb components" =
  let open Structural in
  let circuit =
    create_circuit "hardcaml_comb" (fun () ->
      let open Lib () in
      let a = mk_input "a" 8 in
      let b = mk_input "b" 8 in
      let c = mk_output "c" 1 in
      let n = ~:a in
      let m = a +: b in
      let o = a *+ b in
      let p = ((n |: m) @: (m &: n)) ^: o in
      c <-- (p <: o))
  in
  to_verilog circuit |> print_rope;
  [%expect
    {|
    module hardcaml_comb
    (
      input [7:0] a,
      input [7:0] b,
      output c
    );

      wire _1;
      wire _2;
      wire [7:0] _6;
      wire [7:0] _8;
      wire [15:0] _10;
      wire [7:0] _12;
      wire [7:0] _14;
      wire [15:0] _16;
      wire [15:0] _17;
      wire _19;
      assign _1 = 1'b1;
      assign _2 = 1'b0;
      assign _16 = { _14, _12 };
      assign c = _19;
      hardcaml_lib_not_8 _7
      (
        .i(a),
        .o(_6)
      );
      hardcaml_lib_add_8 _9
      (
        .i0(a),
        .i1(b),
        .o(_8)
      );
      hardcaml_lib_muls_8_8 _11
      (
        .i0(a),
        .i1(b),
        .o(_10)
      );
      hardcaml_lib_and_8 _13
      (
        .i0(_8),
        .i1(_6),
        .o(_12)
      );
      hardcaml_lib_or_8 _15
      (
        .i0(_6),
        .i1(_8),
        .o(_14)
      );
      hardcaml_lib_xor_16 _18
      (
        .i0(_16),
        .i1(_10),
        .o(_17)
      );
      hardcaml_lib_lt_16 _20
      (
        .i0(_17),
        .i1(_10),
        .o(_19)
      );
    endmodule
    |}];
  structural_rtl_components circuit
  |> Set.iter ~f:(fun c -> Structural_rtl_component.rtl_circuit c |> Rtl.print Verilog);
  [%expect
    {|
    module hardcaml_lib_and_8 (
        i1,
        i0,
        o
    );

        input [7:0] i1;
        input [7:0] i0;
        output [7:0] o;

        wire [7:0] _4;
        assign _4 = i0 & i1;
        assign o = _4;

    endmodule
    module hardcaml_lib_or_8 (
        i1,
        i0,
        o
    );

        input [7:0] i1;
        input [7:0] i0;
        output [7:0] o;

        wire [7:0] _4;
        assign _4 = i0 | i1;
        assign o = _4;

    endmodule
    module hardcaml_lib_xor_16 (
        i1,
        i0,
        o
    );

        input [15:0] i1;
        input [15:0] i0;
        output [15:0] o;

        wire [15:0] _4;
        assign _4 = i0 ^ i1;
        assign o = _4;

    endmodule
    module hardcaml_lib_add_8 (
        i1,
        i0,
        o
    );

        input [7:0] i1;
        input [7:0] i0;
        output [7:0] o;

        wire [7:0] _4;
        assign _4 = i0 + i1;
        assign o = _4;

    endmodule
    module hardcaml_lib_lt_16 (
        i1,
        i0,
        o
    );

        input [15:0] i1;
        input [15:0] i0;
        output o;

        wire _4;
        assign _4 = i0 < i1;
        assign o = _4;

    endmodule
    module hardcaml_lib_not_8 (
        i,
        o
    );

        input [7:0] i;
        output [7:0] o;

        wire [7:0] _3;
        assign _3 = ~ i;
        assign o = _3;

    endmodule
    module hardcaml_lib_muls_8_8 (
        i1,
        i0,
        o
    );

        input [7:0] i1;
        input [7:0] i0;
        output [15:0] o;

        wire [15:0] _4;
        assign _4 = $signed(i0) * $signed(i1);
        assign o = _4;

    endmodule
    |}]
;;

let%expect_test "structural rtl reg components" =
  let open Structural in
  let circuit =
    create_circuit "hardcaml_regs" (fun () ->
      let open Lib () in
      let clock = mk_input "clock" 1 in
      let reset = mk_input "reset" 1 in
      let clear = mk_input "clear" 1 in
      let enable = mk_input "enable" 1 in
      let d1 = mk_input "d1" 1 in
      let d8 = mk_input "d8" 8 in
      let q0 = mk_output "q0" 1 in
      q0 <-- reg ~clock d1;
      let q1 = mk_output "q1" 8 in
      q1 <-- reg ~clock ~reset ~reset_to:(Bits.ones 8) d8;
      let q2 = mk_output "q2" 1 in
      q2 <-- reg ~clock ~clear d1;
      let q3 = mk_output "q3" 1 in
      q3
      <-- reg
            ~clock
            ~clock_edge:Falling
            ~reset
            ~reset_edge:Falling
            ~clear
            ~clear_to:(ones 8)
            ~enable
            d8)
  in
  to_verilog circuit |> print_rope;
  [%expect
    {|
    module hardcaml_regs
    (
      input clock,
      input reset,
      input clear,
      input enable,
      input d1,
      input [7:0] d8,
      output q0,
      output [7:0] q1,
      output q2,
      output q3
    );

      wire _1;
      wire _2;
      wire _10;
      wire [7:0] _13;
      wire [7:0] _14;
      wire _17;
      wire [7:0] _20;
      wire [7:0] _21;
      assign _1 = 1'b1;
      assign _2 = 1'b0;
      assign _14 = 8'b00000000;
      assign _20 = 8'b11111111;
      assign q0 = _10;
      assign q1 = _13;
      assign q2 = _17;
      assign q3 = _21;
      hardcaml_lib_reg_1_rr_X_X _11
      (
        .clock(clock),
        .reset(_2),
        .clear(_2),
        .clear_to(_2),
        .enable(_1),
        .d(d1),
        .q(_10)
      );
      hardcaml_lib_reg_8_rr_11111111_X _15
      (
        .clock(clock),
        .reset(reset),
        .clear(_2),
        .clear_to(_14),
        .enable(_1),
        .d(d8),
        .q(_13)
      );
      hardcaml_lib_reg_1_rr_X_X _18
      (
        .clock(clock),
        .reset(_2),
        .clear(clear),
        .clear_to(_2),
        .enable(_1),
        .d(d1),
        .q(_17)
      );
      hardcaml_lib_reg_8_ff_X_X _22
      (
        .clock(clock),
        .reset(reset),
        .clear(clear),
        .clear_to(_20),
        .enable(enable),
        .d(d8),
        .q(_21)
      );
    endmodule
    |}];
  structural_rtl_components circuit
  |> Set.iter ~f:(fun c -> Structural_rtl_component.rtl_circuit c |> Rtl.print Verilog);
  [%expect
    {|
    module hardcaml_lib_reg_1_rr_X_X (
        enable,
        clear_to,
        clear,
        reset,
        clock,
        d,
        q
    );

        input enable;
        input clear_to;
        input clear;
        input reset;
        input clock;
        input d;
        output q;

        wire _8;
        reg _9;
        assign _8 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _9 <= _8;
            else
                if (clear)
                    _9 <= clear_to;
                else
                    if (enable)
                        _9 <= d;
        end
        assign q = _9;

    endmodule
    module hardcaml_lib_reg_8_ff_X_X (
        enable,
        clear_to,
        clear,
        reset,
        clock,
        d,
        q
    );

        input enable;
        input [7:0] clear_to;
        input clear;
        input reset;
        input clock;
        input [7:0] d;
        output [7:0] q;

        wire [7:0] _8;
        reg [7:0] _9;
        assign _8 = 8'b00000000;
        always @(negedge clock or negedge reset) begin
            if (reset == 0)
                _9 <= _8;
            else
                if (clear)
                    _9 <= clear_to;
                else
                    if (enable)
                        _9 <= d;
        end
        assign q = _9;

    endmodule
    module hardcaml_lib_reg_8_rr_11111111_X (
        enable,
        clear_to,
        clear,
        reset,
        clock,
        d,
        q
    );

        input enable;
        input [7:0] clear_to;
        input clear;
        input reset;
        input clock;
        input [7:0] d;
        output [7:0] q;

        wire [7:0] _8;
        reg [7:0] _9;
        assign _8 = 8'b11111111;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _9 <= _8;
            else
                if (clear)
                    _9 <= clear_to;
                else
                    if (enable)
                        _9 <= d;
        end
        assign q = _9;

    endmodule
    |}]
;;
