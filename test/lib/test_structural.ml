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

let%expect_test "Prints with circuit created via With_interface.create_circuit" =
  let open Structural.With_interface (I) (O) (T) in
  (* Simple example with attributes *)
  Structural.write_verilog
    Stdio.print_string
    (create_circuit "foo" (fun i o t ->
       Structural.add_attribute i.a (Rtl_attribute.Vivado.io_buffer_type `IBUF);
       Structural.add_attribute o.b (Rtl_attribute.Vivado.io_buffer_type `OBUF);
       Structural.add_attribute t.c (Rtl_attribute.Vivado.io_buffer_type `None);
       ()));
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
  Structural.write_verilog
    Stdio.print_string
    (create_circuit "bar" (fun i o t ->
       let open Structural in
       t.c <== mux (select i.a 0 0) [ z 1; select i.a 1 1 ];
       o.b <== concat_msb [ select i.a 0 0; i.a ]));
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
  Structural.write_verilog
    Stdio.print_string
    (create_circuit "baz" (fun i o t -> inst "inner_baz" i o t));
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

module Structural_sexp_of_test (Base : Comb.Primitives with type t = Structural.signal) =
struct
  let test name =
    Structural.start_circuit name;
    let module B = Comb.Make (Base) in
    let open Structural in
    let open B in
    let a = mk_input "a" 8 in
    let b = mk_input "b" 8 in
    let const_o = mk_output "const" 8 in
    let sum_o = mk_output "sum" 8 in
    let cat_o = mk_output "cat" 16 in
    let sel_o = mk_output "sel" 4 in
    let mux_o = mk_output "mux" 8 in
    let const = of_int ~width:8 123 in
    let sum = a +: b in
    let cat = a @: b in
    let sel = select a 3 0 in
    let mux = mux (select a 1 0) [ a; b; const ] in
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
    tri_indirect <== tri_wire;
    out_indirect <== out_wire;
    const_o <== const;
    sum_o <== sum;
    cat_o <== cat;
    sel_o <== sel;
    mux_o <== mux;
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

let%expect_test "Structural.Base0 sexp_of_t" =
  let module T = Structural_sexp_of_test (Structural.Base0) in
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

(* {[
     let%expect_test "Structural.Base1 sexp_of_t" =
       let module T = Structural_test (Structural.Base1) in
       T.test "base1";
       [%expect {|
    (Module_input
      (id    7)
      (name  a)
      (width 8))
    (Constant
      (id    14)
      (width 8)
      (value 01111011))
    (Internal_wire
      (id    17)
      (width 16))
    (Internal_wire
      (id    19)
      (width 4))
    (Internal_wire
      (id    29)
      (width 8))
    (Internal_wire
      (id    15)
      (width 8))
    (Module_output
      (id    10)
      (name  sum)
      (width 8))
    (Internal_triwire
      (id    34)
      (width 2))
    (Module_tristate
      (id    35)
      (name  tri_direct)
      (width 2))
    (Module_tristate
      (id    36)
      (name  tri_indirect)
      (width 2))
    (Internal_wire
      (id    31)
      (width 5))
    (Module_output
      (id    32)
      (name  out_direct)
      (width 5))
    (Module_output
      (id    33)
      (name  out_indirect)
      (width 5)) |}]
   ]} *)

(* {[
     let%expect_test "Structural.Base2 sexp_of_t" =
       let module T = Structural_test (Structural.Base2) in
       T.test "base2";
       [%expect {|
    (Module_input
      (id    9)
      (name  a)
      (width 8))
    (Internal_wire
      (id    28)
      (width 8))
    (Internal_wire
      (id    32)
      (width 16))
    (Internal_wire
      (id    34)
      (width 4))
    (Internal_wire
      (id    44)
      (width 8))
    (Internal_wire
      (id    30)
      (width 8))
    (Module_output
      (id    12)
      (name  sum)
      (width 8))
    (Internal_triwire
      (id    49)
      (width 2))
    (Module_tristate
      (id    50)
      (name  tri_direct)
      (width 2))
    (Module_tristate
      (id    51)
      (name  tri_indirect)
      (width 2))
    (Internal_wire
      (id    46)
      (width 5))
    (Module_output
      (id    47)
      (name  out_direct)
      (width 5))
    (Module_output
      (id    48)
      (name  out_indirect)
      (width 5)) |}] ]} *)
