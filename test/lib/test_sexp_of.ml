open! Import

let%expect_test "IntbitsList" =
  print_s IntbitsList.(of_int ~width:8 127 |> sexp_of_t);
  [%expect {| 01111111 |}]
;;

let%expect_test "Bits" =
  print_s Bits.(of_int ~width:3 3 |> sexp_of_t);
  [%expect {| 011 |}]
;;

let%expect_test "Bits" =
  print_s Bits.(of_int ~width:67 (-2) |> sexp_of_t);
  [%expect {| 1111111111111111111111111111111111111111111111111111111111111111110 |}]
;;

let%expect_test "Mutable.ArraybitsInt" =
  print_s Bits.Mutable.Comb.(of_int ~width:13 294 |> sexp_of_t);
  [%expect {| 0000100100110 |}]
;;

let%expect_test "Mutable.ArraybitsInt" =
  print_s Bits.Mutable.Comb.(of_int ~width:19 (-30) |> sexp_of_t);
  [%expect {| 1111111111111100010 |}]
;;

open Signal
open Signal.Unoptimized (* so that primitives are print as defined. *)

let print_signal signal = print_s [%sexp (signal : Signal.t)]

let%expect_test "empty" =
  print_signal empty;
  [%expect {| empty |}]
;;

let%expect_test "simple constant" =
  print_signal (of_int ~width:2 2);
  [%expect {|
    (const
      (width 2)
      (value 0b10)) |}]
;;

let%expect_test "named constant" =
  print_signal vdd;
  [%expect {|
    (const
      (names (vdd))
      (width 1)
      (value 0b1)) |}]
;;

let%expect_test "large constant" =
  print_signal (of_int ~width:9 0x123);
  [%expect {|
    (const
      (width 9)
      (value 0x123)) |}]
;;

let%expect_test "unassigned wire" =
  print_signal (wire 1);
  [%expect {|
    (wire
      (width   1)
      (data_in empty)) |}]
;;

let%expect_test "assigned wire" =
  let w = wire 1 in
  w <== vdd;
  print_signal w;
  [%expect {|
    (wire
      (width   1)
      (data_in 0b1)) |}]
;;

let%expect_test "multiple names" =
  print_signal (wire 1 -- "foo" -- "bar");
  [%expect
    {|
    (wire
      (names (bar foo))
      (width   1)
      (data_in empty)) |}]
;;

let%expect_test "multiple names in arg" =
  let w = wire 1 in
  w <== wire 1 -- "foo" -- "bar";
  print_signal w;
  [%expect {|
    (wire (width 1) (data_in (bar foo))) |}]
;;

let%expect_test "binary ops" =
  let a, b = input "a" 4, of_bit_string "1101" in
  print_signal (a +: b);
  print_signal (a -: b);
  print_signal (a *: b);
  print_signal (a *+ b);
  print_signal (a &: b);
  print_signal (a |: b);
  print_signal (a ^: b);
  print_signal ~:a;
  print_signal (a <: b);
  print_signal (a ==: b);
  print_signal (concat_msb [ a; b; a ]);
  [%expect
    {|
    (add (width 4) (arguments (a 0b1101)))
    (sub (width 4) (arguments (a 0b1101)))
    (mulu (width 8) (arguments (a 0b1101)))
    (muls (width 8) (arguments (a 0b1101)))
    (and (width 4) (arguments (a 0b1101)))
    (or (width 4) (arguments (a 0b1101)))
    (xor (width 4) (arguments (a 0b1101)))
    (not (width 4) (arguments (a)))
    (lt (width 1) (arguments (a 0b1101)))
    (eq (width 1) (arguments (a 0b1101)))
    (cat (width 12) (arguments (a 0b1101 a))) |}]
;;

let%expect_test "printing at leaves" =
  let a, b = input "a" 2, input "b" 2 in
  List.iter
    ~f:print_signal
    [ wireof (a +: b)
    ; zero 2 +: (a -: b)
    ; ~:(a *: b)
    ; input "c" 4 +: (a *+ b)
    ; (a ==: b) -- "eq" ==: (a ==: b)
    ; wireof (a <: b)
    ; wireof ~:a
    ; wireof (mux2 vdd a b)
    ; wireof (bit a 1)
    ; wireof (concat_msb [ a; b ])
    ; wireof (reg (Reg_spec.create () ~clock) ~enable:empty a)
    ; wireof
        (memory
           4
           ~write_port:
             { write_clock = clock
             ; write_address = a
             ; write_enable = bit b 1
             ; write_data = b
             }
           ~read_address:a)
    ];
  [%expect
    {|
    (wire
      (width   2)
      (data_in add))
    (add (width 2) (arguments (0b00 sub)))
    (not (width 4) (arguments (mulu)))
    (add (width 4) (arguments (c muls)))
    (eq (width 1) (arguments (eq eq)))
    (wire
      (width   1)
      (data_in lt))
    (wire
      (width   2)
      (data_in not))
    (wire
      (width   2)
      (data_in mux))
    (wire
      (width   1)
      (data_in select))
    (wire
      (width   4)
      (data_in cat))
    (wire
      (width   2)
      (data_in register))
    (wire
      (width   2)
      (data_in memory)) |}]
;;

let%expect_test "printing at leaves - different types" =
  let a, b = input "a" 2, input "b" 2 in
  print_signal (concat_msb [ concat_msb [ a; b ]; wire 2 -- "cat"; a -- "cat" ]);
  [%expect {|
    (cat (width 8) (arguments (cat cat (cat a)))) |}]
;;

let%expect_test "mux" =
  print_signal (mux (input "sel" 2) (List.init 4 ~f:(of_int ~width:16)));
  [%expect
    {|
    (mux
      (width  16)
      (select sel)
      (data (0x0000 0x0001 0x0002 0x0003))) |}]
;;

let%expect_test "big mux" =
  print_signal (mux (input "sel" 4) (List.init 16 ~f:(of_int ~width:8)));
  [%expect
    {|
    (mux
      (width  8)
      (select sel)
      (data (
        0b00000000
        0b00000001
        0b00000010
        0b00000011
        0b00000100
        0b00000101
        0b00000110
        0b00000111
        0b00001000
        0b00001001
        0b00001010
        0b00001011
        0b00001100
        0b00001101
        0b00001110
        0b00001111))) |}]
;;

let%expect_test "select" =
  print_signal (select (of_int ~width:4 2) 3 2);
  [%expect {|
    (select (width 2) (range (3 2)) (data_in 0b0010)) |}]
;;

let%expect_test "reg r_none" =
  print_signal (reg (Reg_spec.create () ~clock) ~enable:empty (input "a" 1));
  [%expect
    {|
    (register
      (width 1)
      ((clock      clock)
       (clock_edge Rising)
       (enable     0b1))
      (data_in a)) |}]
;;

let%expect_test "reg r_async" =
  print_signal (reg (Reg_spec.create () ~clock ~reset) ~enable:empty (input "a" 1));
  [%expect
    {|
    (register
      (width 1)
      ((clock      clock)
       (clock_edge Rising)
       (reset      reset)
       (reset_edge Rising)
       (reset_to   0b0)
       (enable     0b1))
      (data_in a)) |}]
;;

let%expect_test "reg r_sync" =
  print_signal
    (reg
       (Reg_spec.override (Reg_spec.create () ~clock ~clear) ~clock_edge:Falling)
       ~enable:empty
       (input "a" 1));
  [%expect
    {|
    (register
      (width 1)
      ((clock       clock)
       (clock_edge  Falling)
       (clear       clear)
       (clear_level High)
       (clear_to    0b0)
       (enable      0b1))
      (data_in a)) |}]
;;

let%expect_test "reg r_full" =
  print_signal
    (reg (Reg_spec.create () ~clock ~clear ~reset) ~enable:empty (input "a" 1));
  [%expect
    {|
    (register
      (width 1)
      ((clock       clock)
       (clock_edge  Rising)
       (reset       reset)
       (reset_edge  Rising)
       (reset_to    0b0)
       (clear       clear)
       (clear_level High)
       (clear_to    0b0)
       (enable      0b1))
      (data_in a)) |}]
;;

let%expect_test "memory" =
  print_signal
    (memory
       16
       ~write_port:
         { write_clock = clock
         ; write_enable = input "we" 1
         ; write_address = input "w" 4
         ; write_data = input "d" 32
         }
       ~read_address:(input "r" 4));
  [%expect
    {|
    (memory
      (width 32)
      ((clock      clock)
       (clock_edge Rising)
       (enable     we))
      ((size          16)
       (write_address w)
       (read_address  r)
       (write_enable  we))
      (data_in d)) |}]
;;

let%expect_test "test depth" =
  Signal.sexp_of_signal_recursive ~depth:3 (input "a" 1 <=: input "b" 1) |> print_s;
  [%expect
    {|
    (not
      (width 1)
      (arguments ((
        lt
        (width 1)
        (arguments (
          (wire
            (names (b))
            (width   1)
            (data_in empty))
          (wire
            (names (a))
            (width   1)
            (data_in empty)))))))) |}]
;;

let%expect_test "test instantiation" =
  Signal.sexp_of_signal_recursive
    ~depth:2
    ((Instantiation.create
        ()
        ~name:"module_name"
        ~parameters:
          [ Parameter.create ~name:"int_param" ~value:(Int 1)
          ; Parameter.create ~name:"string_param" ~value:(String "string_value")
          ; Parameter.create ~name:"bool_param" ~value:(Bool true)
          ; Parameter.create ~name:"float_param" ~value:(Real 1.2)
          ]
        ~inputs:[ "i1", input "a" 3; "i2", input "b" 2 ]
        ~outputs:[ "o1", 4; "o2", 3 ])
     #o
       "o2")
  |> print_s;
  [%expect
    {|
    (select
      (width 3)
      (range (6 4))
      (data_in (
        instantiation
        (width 7)
        ("work.module_name(rtl)"
          (parameters (
            (int_param    1)
            (string_param string_value)
            (bool_param   true)
            (float_param  1.2)))
          (inputs  ((i1 a) (i2 b)))
          (outputs ((o2 3) (o1 4))))))) |}]
;;

(* Structural.sexp_of *)

module Structural_test (Base : Comb.Primitives with type t = Structural.signal) = struct
  let test name =
    Structural.circuit name;
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

  (*write_verilog print_string (find_circuit name); *)
end

let%expect_test "Structural.Base0 sexp_of_t" =
  let module T = Structural_test (Structural.Base0) in
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
      (width 5)) |}]
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
