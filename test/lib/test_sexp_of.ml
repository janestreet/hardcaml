open! Import
module Bits_list = Bits_list.Int_comb

let%expect_test "Bits_list" =
  print_s Bits_list.(of_int ~width:8 127 |> sexp_of_t);
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
      (value 0b10))
    |}]
;;

let%expect_test "named constant" =
  print_signal vdd;
  [%expect {|
    (const
      (names (vdd))
      (width 1)
      (value 0b1))
    |}]
;;

let%expect_test "large constant" =
  print_signal (of_int ~width:9 0x123);
  [%expect {|
    (const
      (width 9)
      (value 0x123))
    |}]
;;

let%expect_test "unassigned wire" =
  print_signal (wire 1);
  [%expect {|
    (wire
      (width   1)
      (data_in empty))
    |}]
;;

let%expect_test "assigned wire" =
  let w = wire 1 in
  w <== vdd;
  print_signal w;
  [%expect {|
    (wire
      (width   1)
      (data_in 0b1))
    |}]
;;

let%expect_test "multiple names" =
  print_signal (wire 1 -- "foo" -- "bar");
  [%expect
    {|
    (wire
      (names (bar foo))
      (width   1)
      (data_in empty))
    |}]
;;

let%expect_test "multiple names in arg" =
  let w = wire 1 in
  w <== wire 1 -- "foo" -- "bar";
  print_signal w;
  [%expect {| (wire (width 1) (data_in (bar foo))) |}]
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
    (cat (width 12) (arguments (a 0b1101 a)))
    |}]
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
      (data_in memory_read_port))
    |}]
;;

let%expect_test "printing at leaves - different types" =
  let a, b = input "a" 2, input "b" 2 in
  print_signal (concat_msb [ concat_msb [ a; b ]; wire 2 -- "cat"; a -- "cat" ]);
  [%expect {| (cat (width 8) (arguments (cat cat (cat a)))) |}]
;;

let%expect_test "mux" =
  print_signal (mux (input "sel" 2) (List.init 4 ~f:(of_int ~width:16)));
  [%expect
    {|
    (mux
      (width  16)
      (select sel)
      (data (0x0000 0x0001 0x0002 0x0003)))
    |}]
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
        0b00001111)))
    |}]
;;

let%expect_test "select" =
  print_signal (select (of_int ~width:4 2) 3 2);
  [%expect {| (select (width 2) (range (3 2)) (data_in 0b0010)) |}]
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
      (data_in a))
    |}]
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
      (data_in a))
    |}]
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
      (data_in a))
    |}]
;;

let%expect_test "reg r_full" =
  print_signal (reg (Reg_spec.create () ~clock ~clear ~reset) ~enable:empty (input "a" 1));
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
      (data_in a))
    |}]
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
    (memory_read_port
      (width 32)
      ((memory         multiport_memory)
       (read_addresses r)))
    |}]
;;

let%expect_test "test depth" =
  Signal.Type.sexp_of_signal_recursive ~depth:3 (input "a" 1 <=: input "b" 1) |> print_s;
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
            (data_in empty))))))))
    |}]
;;

let%expect_test "test instantiation" =
  Signal.Type.sexp_of_signal_recursive
    ~depth:2
    (Map.find_exn
       (Instantiation.create
          ()
          ~name:"module_name"
          ~parameters:
            [ Parameter.create ~name:"int_param" ~value:(Int 1)
            ; Parameter.create ~name:"string_param" ~value:(String "string_value")
            ; Parameter.create ~name:"bool_param" ~value:(Bool true)
            ; Parameter.create ~name:"float_param" ~value:(Real 1.2)
            ; Parameter.create
                ~name:"bit_vector_param"
                ~value:(Bit_vector (Logic.Bit_vector.of_string "11001"))
            ]
          ~inputs:[ "i1", input "a" 3; "i2", input "b" 2 ]
          ~outputs:[ "o1", 4; "o2", 3 ])
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
        ("work.module_name(rtl){the_module_name}"
          (parameters (
            (int_param        (Int        1))
            (string_param     (String     string_value))
            (bool_param       (Bool       true))
            (float_param      (Real       1.2))
            (bit_vector_param (Bit_vector 11001))))
          (inputs  ((i1 a) (i2 b)))
          (outputs ((o2 3) (o1 4)))))))
    |}]
;;

(* Structural.sexp_of *)
