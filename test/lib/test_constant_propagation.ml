open! Import
open Signal

let iter_all_inputs ~min_width ~max_width ~f =
  for width = min_width to max_width do
    for n = 0 to (1 lsl width) - 1 do
      f (of_int ~width n)
    done
  done
;;

module Trace = struct
  type 'a fn1 = 'a * Signal.t
  type ('a, 'b) fn2 = ('a * 'b) * Signal.t
  type ('a, 'b, 'c) fn3 = ('a * 'b * 'c) * Signal.t
  type signal = Signal.t

  let fn1 f a = a, f a
  let fn2 f a b = (a, b), f a b
  let fn3 f a b c = (a, b, c), f a b c
  let sexp_of_signal = Test_constants.sexp_of_const_signal ?depth:None
  let sexp_of_fn1 sexp_of_a (a, r) = [%message "" ~_:(a : a) ~_:"=" ~_:(r : signal)]

  let sexp_of_fn2 sexp_of_a sexp_of_b ((a, b), r) =
    [%message "" ~_:(a, b : a * b) ~_:"=" ~_:(r : signal)]
  ;;

  let sexp_of_fn3 sexp_of_a sexp_of_b sexp_of_c ((a, b, c), r) =
    [%message "" ~_:(a, b, c : a * b * c) ~_:"=" ~_:(r : signal)]
  ;;

  let sexp_of_op2 op sexp_of_b ((a, b), r) =
    [%message "" ~_:(a : signal) ~_:(op : string) ~_:(b : b) "=" ~_:(r : signal)]
  ;;

  let%expect_test "test trace" =
    let sexp_of_op2_and = sexp_of_op2 "&:" in
    print_s [%message "~:" ~_:(fn1 ( ~: ) (of_bit_string "10") : signal fn1)];
    print_s [%message "+:" ~_:(fn2 ( +: ) vdd gnd : (signal, signal) fn2)];
    print_s [%message "" ~_:(fn2 ( &: ) vdd gnd : signal op2_and)];
    print_s [%message "mux2" ~_:(fn3 mux2 vdd gnd vdd : (signal, signal, signal) fn3)];
    print_s [%message "input" ~_:(fn2 Signal.input "a" 1 : (string, int) fn2)];
    [%expect
      {|
      (~: (2'b10 = 2'b01))
      (+: ((1'b1 1'b0) = 1'b1))
      (1'b1 &: 1'b0 = 1'b0)
      (mux2 ((1'b1 1'b0 1'b1) = 1'b0))
      (input ((a 1) = a)) |}]
  ;;

  (* operator tests *)

  let op2 bits ~f =
    List.concat
    @@ List.init (1 lsl bits) ~f:(fun x ->
      List.init (1 lsl bits) ~f:(fun y ->
        f (of_int ~width:bits x) (of_int ~width:bits y)))
  ;;

  let op1 bits ~f = List.init (1 lsl bits) ~f:(fun y -> f (of_int ~width:bits y))

  let binary_op_tests name ( +: ) ( +:. ) =
    let ( +: ), ( +:. ) = fn2 ( +: ), fn2 ( +:. ) in
    let sexp_of_op2 f s = sexp_of_op2 name f s in
    [%message
      name
        ~all_1_bit:(op2 1 ~f:( +: ) : signal op2 list)
        ~all_2_bits:(op2 2 ~f:( +: ) : signal op2 list)
        ~misc:
          ([ of_int ~width:8 22 +: of_int ~width:8 33
           ; of_int ~width:123 22345 +: of_int ~width:123 (-22345)
           ]
           : signal op2 list)
        ~int_on_right:
          ([ of_int ~width:7 27 +:. 12; of_int ~width:7 27 +:. -12 ] : int op2 list)]
  ;;

  let binary_op_tests_no_rhs_int name ( +: ) =
    let ( +: ) = fn2 ( +: ) in
    let sexp_of_op2 f s = sexp_of_op2 name f s in
    [%message
      name
        ~all_1_bit:(op2 1 ~f:( +: ) : signal op2 list)
        ~all_2_bits:(op2 2 ~f:( +: ) : signal op2 list)
        ~misc:
          ([ of_int ~width:8 22 +: of_int ~width:8 33
           ; of_int ~width:123 22345 +: of_int ~width:123 (-22345)
           ]
           : signal op2 list)]
  ;;

  let binary_op_tests_with_one_constant name f =
    iter_all_inputs ~min_width:1 ~max_width:2 ~f:(fun c ->
      let sexp_of_t = Test_constants.sexp_of_const_signal ~depth:Int.max_value in
      let x = Signal.wire (width c) -- "x" in
      print_s [%message "" ~_:(c : t) name ~_:(x : t) "=" ~_:(f c x : t)];
      print_s [%message "" ~_:(x : t) name ~_:(c : t) "=" ~_:(f x c : t)])
  ;;

  let mul_op_tests name ( *: ) =
    let ( *: ) = fn2 ( *: ) in
    let sexp_of_op2 f s = sexp_of_op2 name f s in
    [%message
      name
        ~all_1_bit:(op2 1 ~f:( *: ) : signal op2 list)
        ~all_2_bits:(op2 2 ~f:( *: ) : signal op2 list)
        ~pow2:
          ([ of_int ~width:2 3 *: of_int ~width:5 1
           ; of_int ~width:2 3 *: of_int ~width:5 2
           ; of_int ~width:2 3 *: of_int ~width:5 4
           ; of_int ~width:2 3 *: of_int ~width:5 8
           ; of_int ~width:2 3 *: of_int ~width:5 16
           ]
           : signal op2 list)
        ~misc:
          ([ of_int ~width:8 22 *: of_int ~width:8 33
           ; of_int ~width:7 27 *: of_int ~width:4 12
           ]
           : signal op2 list)]
  ;;
end

open Trace

let show signal = print_s [%sexp (signal : signal)]

let%expect_test "concat" =
  print_s
    [%message
      ""
        ~concat:
          (List.map
             ~f:(fn1 concat_msb)
             [ [ vdd; gnd ]; [ vdd; gnd; vdd ]; [ one 4; ones 3; zero 2 ] ]
           : signal list fn1 list)
        ~concat_e:
          (List.map
             ~f:(fn1 concat_msb_e)
             [ [ vdd; empty ]
             ; [ empty; vdd ]
             ; [ empty; vdd; empty; gnd ]
             ; [ of_int ~width:3 2; empty; of_int ~width:2 1 ]
             ]
           : signal list fn1 list)
        ~concat_op:(binary_op_tests_no_rhs_int "@:" ( @: ) : Sexp.t)];
  [%expect
    {|
    ((concat (
       ((1'b1 1'b0) = 2'b10)
       ((1'b1    1'b0   1'b1)  = 3'b101)
       ((4'b0001 3'b111 2'b00) = 9'h03c)))
     (concat_e (
       ((1'b1  empty) = 1'b1)
       ((empty 1'b1)  = 1'b1)
       ((empty 1'b1 empty 1'b0) = 2'b10)
       ((3'b010 empty 2'b01) = 5'b01001)))
     (concat_op (
       @:
       (all_1_bit (
         (1'b0 @: 1'b0 = 2'b00)
         (1'b0 @: 1'b1 = 2'b01)
         (1'b1 @: 1'b0 = 2'b10)
         (1'b1 @: 1'b1 = 2'b11)))
       (all_2_bits (
         (2'b00 @: 2'b00 = 4'b0000)
         (2'b00 @: 2'b01 = 4'b0001)
         (2'b00 @: 2'b10 = 4'b0010)
         (2'b00 @: 2'b11 = 4'b0011)
         (2'b01 @: 2'b00 = 4'b0100)
         (2'b01 @: 2'b01 = 4'b0101)
         (2'b01 @: 2'b10 = 4'b0110)
         (2'b01 @: 2'b11 = 4'b0111)
         (2'b10 @: 2'b00 = 4'b1000)
         (2'b10 @: 2'b01 = 4'b1001)
         (2'b10 @: 2'b10 = 4'b1010)
         (2'b10 @: 2'b11 = 4'b1011)
         (2'b11 @: 2'b00 = 4'b1100)
         (2'b11 @: 2'b01 = 4'b1101)
         (2'b11 @: 2'b10 = 4'b1110)
         (2'b11 @: 2'b11 = 4'b1111)))
       (misc (
         (8'b00010110 @: 8'b00100001 = 16'h1621)
         (123'h0000000000000000000000000005749
          @:
          123'h7ffffffffffffffffffffffffffa8b7
          =
          246'h0000000000000000000000000002ba4fffffffffffffffffffffffffffa8b7)))))) |}];
  [%expect {| |}]
;;

let%expect_test "concat empty list" =
  show_raise (fun () -> concat_msb []);
  [%expect {|
    (raised "[concat] got empty list") |}];
  show_raise (fun () -> concat_msb_e []);
  [%expect {|
    (raised "[concat] got empty list") |}]
;;

let%expect_test "concat_msb empty signal" =
  show_raise (fun () -> concat_msb [ empty ]);
  [%expect {|
    (raised ("[concat] got [empty] input" (empty))) |}]
;;

let%expect_test "[@:] empty signal" =
  show_raise (fun () -> empty @: empty);
  [%expect {|
    (raised ("[concat] got [empty] input" (empty empty))) |}];
  show_raise (fun () -> empty @: vdd);
  [%expect
    {|
    (raised (
      "[concat] got [empty] input" (
        empty (
          const
          (names (vdd))
          (width 1)
          (value 0b1))))) |}];
  show_raise (fun () -> vdd @: empty);
  [%expect
    {|
    (raised (
      "[concat] got [empty] input" (
        (const
          (names (vdd))
          (width 1)
          (value 0b1))
        empty))) |}]
;;

let%expect_test "repeat" =
  let repeat = fn2 repeat in
  print_s
    [%message
      "repeat"
        ~_:
          ([ repeat vdd 3; repeat (of_string "01") 4; repeat (of_int ~width:8 123) 1 ]
           : (signal, int) fn2 list)];
  [%expect
    {|
    (repeat (
      ((1'b1        3) = 3'b111)
      ((2'b01       4) = 8'b01010101)
      ((8'b01111011 1) = 8'b01111011))) |}]
;;

let%expect_test "repeat empty" =
  show (repeat empty 1);
  [%expect {|
    empty |}]
;;

let%expect_test "repeat _ 0" =
  show (repeat vdd 0);
  [%expect {|
    empty |}]
;;

let%expect_test "reduce" =
  let d =
    [ [ vdd; gnd ]
    ; [ gnd; gnd; gnd ]
    ; [ vdd; vdd; vdd ]
    ; [ vdd; gnd; gnd ]
    ; [ vdd; gnd; gnd; vdd ]
    ]
  in
  let reduce f = fn1 (reduce ~f) in
  print_s
    [%message
      "reduce"
        ~and_:(List.map ~f:(reduce ( &: )) d : signal list fn1 list)
        ~or_:(List.map ~f:(reduce ( |: )) d : signal list fn1 list)
        ~xor:(List.map ~f:(reduce ( ^: )) d : signal list fn1 list)];
  [%expect
    {|
    (reduce
      (and_ (
        ((1'b1 1'b0) = 1'b0)
        ((1'b0 1'b0 1'b0) = 1'b0)
        ((1'b1 1'b1 1'b1) = 1'b1)
        ((1'b1 1'b0 1'b0) = 1'b0)
        ((1'b1 1'b0 1'b0 1'b1) = 1'b0)))
      (or_ (
        ((1'b1 1'b0) = 1'b1)
        ((1'b0 1'b0 1'b0) = 1'b0)
        ((1'b1 1'b1 1'b1) = 1'b1)
        ((1'b1 1'b0 1'b0) = 1'b1)
        ((1'b1 1'b0 1'b0 1'b1) = 1'b1)))
      (xor (
        ((1'b1 1'b0) = 1'b1)
        ((1'b0 1'b0 1'b0) = 1'b0)
        ((1'b1 1'b1 1'b1) = 1'b1)
        ((1'b1 1'b0 1'b0) = 1'b1)
        ((1'b1 1'b0 1'b0 1'b1) = 1'b0)))) |}]
;;

let%expect_test "reverse" =
  show (reverse (of_string "1110"));
  [%expect {|
    4'b0111 |}]
;;

(* It doesn't make sense if the limit is [>= (1 lsl width)] and an exception should be
   raised. *)
let%expect_test "mod_counter should raise" =
  require_does_raise ~cr:CR_someday [%here] (fun () ->
    mod_counter ~max:8 (of_string "101"));
  [%expect
    {|
    ("mod counter limit is great than max counter value"
     (limit     8)
     (max_value 7)) |}]
;;

let%expect_test "mod_counter" =
  let mod_counter n = fn1 (mod_counter ~max:n) in
  print_s
    [%message
      "mod_counter"
        ~mod_4:
          (List.init 8 ~f:(fun i -> mod_counter 3 (of_int ~width:3 i)) : signal fn1 list)
        ~mod_7:
          (List.init 8 ~f:(fun i -> mod_counter 6 (of_int ~width:3 i)) : signal fn1 list)];
  [%expect
    {|
    (mod_counter
      (mod_4 (
        (3'b000 = 3'b001)
        (3'b001 = 3'b010)
        (3'b010 = 3'b011)
        (3'b011 = 3'b000)
        (3'b100 = 3'b101)
        (3'b101 = 3'b110)
        (3'b110 = 3'b111)
        (3'b111 = 3'b000)))
      (mod_7 (
        (3'b000 = 3'b001)
        (3'b001 = 3'b010)
        (3'b010 = 3'b011)
        (3'b011 = 3'b100)
        (3'b100 = 3'b101)
        (3'b101 = 3'b110)
        (3'b110 = 3'b000)
        (3'b111 = 3'b000)))) |}]
;;

let%expect_test "tree" =
  let tree arity f = fn1 (tree ~arity ~f) in
  print_s
    [%message
      "tree with different branching factors"
        ~add_branch_2:
          (tree
             2
             (reduce ~f:( +: ))
             (List.map ~f:(of_int ~width:10) [ 10; 20; 30; 40; 50; 60; 70 ])
           : signal list fn1)
        ~add_branch_3:
          (tree
             3
             (reduce ~f:( +: ))
             (List.map ~f:(of_int ~width:10) [ 10; 20; 30; 40; 50; 60; 70 ])
           : signal list fn1)];
  [%expect
    {|
    ("tree with different branching factors"
      (add_branch_2 (
        (10'h00a 10'h014 10'h01e 10'h028 10'h032 10'h03c 10'h046) = 10'h118))
      (add_branch_3 (
        (10'h00a 10'h014 10'h01e 10'h028 10'h032 10'h03c 10'h046) = 10'h118))) |}]
;;

let%expect_test "binary_to_onehot" =
  let binary_to_onehot = fn1 binary_to_onehot in
  print_s
    [%message
      "binary_to_onehot"
        ~_:
          (List.init 4 ~f:(fun i -> binary_to_onehot (of_int ~width:2 i))
           : signal fn1 list)];
  [%expect
    {|
    (binary_to_onehot (
      (2'b00 = 4'b0001)
      (2'b01 = 4'b0010)
      (2'b10 = 4'b0100)
      (2'b11 = 4'b1000))) |}]
;;

let%expect_test "onehot_to_binary" =
  let onehot_to_binary = fn1 onehot_to_binary in
  print_s
    [%message
      "onehot_to_binary"
        ~_:
          (List.init 4 ~f:(fun i -> onehot_to_binary (of_int ~width:4 (1 lsl i)))
           : signal fn1 list)];
  [%expect
    {|
    (onehot_to_binary (
      (4'b0001 = 2'b00)
      (4'b0010 = 2'b01)
      (4'b0100 = 2'b10)
      (4'b1000 = 2'b11))) |}]
;;

let%expect_test "gray_to_binary" =
  let gray_to_binary = fn1 gray_to_binary in
  print_s
    [%message
      "gray_to_binary"
        ~_:
          (List.init 8 ~f:(fun i -> gray_to_binary (of_int ~width:3 i))
           : signal fn1 list)];
  [%expect
    {|
    (gray_to_binary (
      (3'b000 = 3'b000)
      (3'b001 = 3'b001)
      (3'b010 = 3'b011)
      (3'b011 = 3'b010)
      (3'b100 = 3'b111)
      (3'b101 = 3'b110)
      (3'b110 = 3'b100)
      (3'b111 = 3'b101))) |}]
;;

let%expect_test "binary_to_gray" =
  let binary_to_gray = fn1 binary_to_gray in
  print_s
    [%message
      "binary_to_gray"
        ~_:
          (List.init 8 ~f:(fun i -> binary_to_gray (of_int ~width:3 i))
           : signal fn1 list)];
  [%expect
    {|
    (binary_to_gray (
      (3'b000 = 3'b000)
      (3'b001 = 3'b001)
      (3'b010 = 3'b011)
      (3'b011 = 3'b010)
      (3'b100 = 3'b110)
      (3'b101 = 3'b111)
      (3'b110 = 3'b101)
      (3'b111 = 3'b100))) |}]
;;

let%expect_test "[binary_to_gray] and [gray_to_binary] are inverses" =
  let num_tests = ref 0 in
  iter_all_inputs ~min_width:1 ~max_width:5 ~f:(fun input ->
    incr num_tests;
    let output = binary_to_gray (gray_to_binary input) in
    require
      [%here]
      (Sexp.equal [%sexp (input : signal)] [%sexp (output : signal)])
      ~if_false_then_print_s:(lazy [%message (input : signal) (output : signal)]));
  print_s [%sexp (!num_tests : int)];
  [%expect {| 62 |}]
;;

let%expect_test "[uresize 0]" =
  require_does_raise [%here] (fun () -> uresize (of_bit_string "0") 0);
  [%expect {|
    ("[select] got [hi < lo]"
      (hi -1)
      (lo 0)) |}]
;;

let%expect_test "[sresize 0]" =
  require_does_raise [%here] (fun () -> sresize (of_bit_string "0") 0);
  [%expect {|
    ("[select] got [hi < lo]"
      (hi -1)
      (lo 0)) |}]
;;

let%expect_test "resizing" =
  let uresize, sresize = fn2 uresize, fn2 sresize in
  print_s
    [%message
      ""
        ~uresize_0:
          (List.init 3 ~f:(fun i -> uresize (of_bit_string "0") (i + 1))
           : (signal, int) fn2 list)
        ~uresize_1:
          (List.init 3 ~f:(fun i -> uresize (of_bit_string "1") (i + 1))
           : (signal, int) fn2 list)
        ~uresize_100:
          (List.init 5 ~f:(fun i -> uresize (of_bit_string "100") (i + 1))
           : (signal, int) fn2 list)
        ~sresize_0:
          (List.init 3 ~f:(fun i -> sresize (of_bit_string "0") (i + 1))
           : (signal, int) fn2 list)
        ~sresize_1:
          (List.init 3 ~f:(fun i -> sresize (of_bit_string "1") (i + 1))
           : (signal, int) fn2 list)
        ~sresize_100:
          (List.init 5 ~f:(fun i -> sresize (of_bit_string "100") (i + 1))
           : (signal, int) fn2 list)
        ~ue:(op1 2 ~f:(fn1 ue) : signal fn1 list)
        ~se:(op1 2 ~f:(fn1 se) : signal fn1 list)];
  [%expect
    {|
    ((uresize_0 (
       ((1'b0 1) = 1'b0)
       ((1'b0 2) = 2'b00)
       ((1'b0 3) = 3'b000)))
     (uresize_1 (
       ((1'b1 1) = 1'b1)
       ((1'b1 2) = 2'b01)
       ((1'b1 3) = 3'b001)))
     (uresize_100 (
       ((3'b100 1) = 1'b0)
       ((3'b100 2) = 2'b00)
       ((3'b100 3) = 3'b100)
       ((3'b100 4) = 4'b0100)
       ((3'b100 5) = 5'b00100)))
     (sresize_0 (
       ((1'b0 1) = 1'b0)
       ((1'b0 2) = 2'b00)
       ((1'b0 3) = 3'b000)))
     (sresize_1 (
       ((1'b1 1) = 1'b1)
       ((1'b1 2) = 2'b11)
       ((1'b1 3) = 3'b111)))
     (sresize_100 (
       ((3'b100 1) = 1'b0)
       ((3'b100 2) = 2'b00)
       ((3'b100 3) = 3'b100)
       ((3'b100 4) = 4'b1100)
       ((3'b100 5) = 5'b11100)))
     (ue (
       (2'b00 = 3'b000)
       (2'b01 = 3'b001)
       (2'b10 = 3'b010)
       (2'b11 = 3'b011)))
     (se (
       (2'b00 = 3'b000)
       (2'b01 = 3'b001)
       (2'b10 = 3'b110)
       (2'b11 = 3'b111)))) |}]
;;

let%expect_test "[select]" =
  let num_tests = ref 0 in
  iter_all_inputs ~min_width:2 ~max_width:5 ~f:(fun input ->
    let width = width input in
    for split = 0 to width - 2 do
      incr num_tests;
      let output =
        concat_msb_e [ select input (width - 1) (split + 1); select input split 0 ]
      in
      require
        [%here]
        (Sexp.equal [%sexp (input : signal)] [%sexp (output : signal)])
        ~if_false_then_print_s:
          (lazy [%message (split : int) (input : signal) (output : signal)])
    done);
  print_s [%sexp (!num_tests : int)];
  [%expect {|
    196 |}]
;;

let%expect_test "select" =
  print_s
    [%message
      "select"
        ~_0_up:
          (List.init 4 ~f:(fun i -> fn3 select (of_string "1100") i 0)
           : (signal, int, int) fn3 list)
        ~_3_down:
          (List.init 4 ~f:(fun i -> fn3 select (of_string "1100") 3 (3 - i))
           : (signal, int, int) fn3 list)
        ~middle:(fn3 select (of_string "0110") 2 1 : (signal, int, int) fn3)
        ~_64_bit_boundary:
          (fn3 select (of_hex ~width:68 "18000000000000000") 65 62
           : (signal, int, int) fn3)
        ~bit:
          (List.init 4 ~f:(fun i -> fn2 bit (of_string "1100") i)
           : (signal, int) fn2 list)
        ~lsbs:(List.init 4 ~f:(fun i -> fn1 lsbs (one (i + 2))) : signal fn1 list)
        ~msbs:(List.init 4 ~f:(fun i -> fn1 msbs (one (i + 2))) : signal fn1 list)
        ~lsb:(List.init 4 ~f:(fun i -> fn1 lsb (one (i + 1))) : signal fn1 list)
        ~msb:(List.init 4 ~f:(fun i -> fn1 msb (one (i + 1))) : signal fn1 list)
        ~drop_bottom:
          (List.init 4 ~f:(fun i -> fn2 drop_bottom (of_string "11100") (i + 1))
           : (signal, int) fn2 list)
        ~drop_top:
          (List.init 4 ~f:(fun i -> fn2 drop_top (of_string "11100") (i + 1))
           : (signal, int) fn2 list)
        ~sel_bottom:
          (List.init 5 ~f:(fun i -> fn2 sel_bottom (of_string "11100") (i + 1))
           : (signal, int) fn2 list)
        ~sel_top:
          (List.init 5 ~f:(fun i -> fn2 sel_top (of_string "11100") (i + 1))
           : (signal, int) fn2 list)
        ~bits_of_101:(bits_msb (of_string "101") : signal list)];
  [%expect
    {|
    (select
      (_0_up (
        ((4'b1100 0 0) = 1'b0)
        ((4'b1100 1 0) = 2'b00)
        ((4'b1100 2 0) = 3'b100)
        ((4'b1100 3 0) = 4'b1100)))
      (_3_down (
        ((4'b1100 3 3) = 1'b1)
        ((4'b1100 3 2) = 2'b11)
        ((4'b1100 3 1) = 3'b110)
        ((4'b1100 3 0) = 4'b1100)))
      (middle           ((4'b0110               2  1)  = 2'b11))
      (_64_bit_boundary ((68'h18000000000000000 65 62) = 4'b0110))
      (bit (
        ((4'b1100 0) = 1'b0)
        ((4'b1100 1) = 1'b0)
        ((4'b1100 2) = 1'b1)
        ((4'b1100 3) = 1'b1)))
      (lsbs (
        (2'b01    = 1'b1)
        (3'b001   = 2'b01)
        (4'b0001  = 3'b001)
        (5'b00001 = 4'b0001)))
      (msbs (
        (2'b01    = 1'b0)
        (3'b001   = 2'b00)
        (4'b0001  = 3'b000)
        (5'b00001 = 4'b0000)))
      (lsb (
        (1'b1    = 1'b1)
        (2'b01   = 1'b1)
        (3'b001  = 1'b1)
        (4'b0001 = 1'b1)))
      (msb (
        (1'b1    = 1'b1)
        (2'b01   = 1'b0)
        (3'b001  = 1'b0)
        (4'b0001 = 1'b0)))
      (drop_bottom (
        ((5'b11100 1) = 4'b1110)
        ((5'b11100 2) = 3'b111)
        ((5'b11100 3) = 2'b11)
        ((5'b11100 4) = 1'b1)))
      (drop_top (
        ((5'b11100 1) = 4'b1100)
        ((5'b11100 2) = 3'b100)
        ((5'b11100 3) = 2'b00)
        ((5'b11100 4) = 1'b0)))
      (sel_bottom (
        ((5'b11100 1) = 1'b0)
        ((5'b11100 2) = 2'b00)
        ((5'b11100 3) = 3'b100)
        ((5'b11100 4) = 4'b1100)
        ((5'b11100 5) = 5'b11100)))
      (sel_top (
        ((5'b11100 1) = 1'b1)
        ((5'b11100 2) = 2'b11)
        ((5'b11100 3) = 3'b111)
        ((5'b11100 4) = 4'b1110)
        ((5'b11100 5) = 5'b11100)))
      (bits_of_101 (1'b1 1'b0 1'b1))) |}]
;;

let%expect_test "select_e sexp_of bug? Yes, fixed." =
  show (select_e vdd 1 1);
  [%expect {|
    empty |}];
  show (select_e gnd (-1) 1);
  [%expect {|
    empty |}]
;;

let%expect_test "insert" =
  print_s
    [%message
      "insert"
        ~insert_010_into_0s:
          (List.init 6 ~f:(fun at_offset ->
             insert ~into:(zero 8) (of_string "010") ~at_offset)
           : signal list)
        ~insert_010_into_1s:
          (List.init 6 ~f:(fun at_offset ->
             insert ~into:(ones 8) (of_string "010") ~at_offset)
           : signal list)];
  [%expect
    {|
    (insert
      (insert_010_into_0s (
        8'b00000010 8'b00000100 8'b00001000 8'b00010000 8'b00100000 8'b01000000))
      (insert_010_into_1s (
        8'b11111010 8'b11110101 8'b11101011 8'b11010111 8'b10101111 8'b01011111))) |}]
;;

let%expect_test "split_in_half" =
  let create b = concat_msb_e [ ones ((b + 1) / 2); zero (b / 2) ] in
  let sexp_of_left_and_right (left, right) =
    [%message "" (left : signal) (right : signal)]
  in
  print_s
    [%message
      "split"
        ~_:
          (List.init 8 ~f:(fun b -> split_in_half_msb (create (b + 2)))
           : left_and_right list)];
  [%expect
    {|
    (split (
      ((left 1'b1)     (right 1'b0))
      ((left 2'b11)    (right 1'b0))
      ((left 2'b11)    (right 2'b00))
      ((left 3'b111)   (right 2'b00))
      ((left 3'b111)   (right 3'b000))
      ((left 4'b1111)  (right 3'b000))
      ((left 4'b1111)  (right 4'b0000))
      ((left 5'b11111) (right 4'b0000)))) |}];
  [%expect {| |}]
;;

let%expect_test "split_lsb" =
  let split ?exact ~part_width t =
    print_s [%sexp (split_lsb ?exact ~part_width t : signal list)]
  in
  let split_raises ~part_width t =
    require_does_raise [%here] (fun () -> split ~part_width t)
  in
  split_raises ~part_width:0 vdd;
  [%expect {| ("[split] got [part_width <= 0]" (part_width 0)) |}];
  split_raises ~part_width:1 empty;
  [%expect {|
    "[split] got [empty] input" |}];
  split ~part_width:1 (of_int ~width:2 1);
  [%expect {| (1'b1 1'b0) |}];
  split ~part_width:2 (of_int ~width:2 1);
  [%expect {| (2'b01) |}];
  split ~part_width:4 (of_int ~width:16 0x4321);
  [%expect {| (4'b0001 4'b0010 4'b0011 4'b0100) |}];
  split_raises ~part_width:4 (of_int ~width:15 0x4321);
  [%expect
    {|
    ("[split ~exact:true] unable to split exactly"
     (input_width        15)
     (part_width         4)
     (width_of_last_part 3)) |}];
  split ~exact:false ~part_width:4 (of_int ~width:15 0x4321);
  [%expect {| (4'b0001 4'b0010 4'b0011 3'b100) |}]
;;

let%expect_test "split_msb" =
  let split ?exact ~part_width t =
    print_s [%sexp (split_msb ?exact ~part_width t : signal list)]
  in
  let split_raises ~part_width t =
    require_does_raise [%here] (fun () -> split ~part_width t)
  in
  split_raises ~part_width:0 vdd;
  [%expect {| ("[split] got [part_width <= 0]" (part_width 0)) |}];
  split_raises ~part_width:1 empty;
  [%expect {|
    "[split] got [empty] input" |}];
  split ~part_width:1 (of_int ~width:2 1);
  [%expect {| (1'b0 1'b1) |}];
  split ~part_width:2 (of_int ~width:2 1);
  [%expect {| (2'b01) |}];
  split ~part_width:4 (of_int ~width:16 0x4321);
  [%expect {| (4'b0100 4'b0011 4'b0010 4'b0001) |}];
  split_raises ~part_width:4 (of_int ~width:15 0x4321);
  [%expect
    {|
    ("[split ~exact:true] unable to split exactly"
     (input_width        15)
     (part_width         4)
     (width_of_last_part 3)) |}];
  split ~exact:false ~part_width:4 (of_int ~width:15 0x4321);
  [%expect {| (4'b1000 4'b0110 4'b0100 3'b001) |}]
;;

let%expect_test "bswap" =
  require_does_raise [%here] (fun () -> bswap vdd);
  require_does_raise [%here] (fun () -> bswap (zero 13));
  [%expect
    {|
    ("bswap argument must be a multiple of 8 bits width" (actual_width 1))
    ("bswap argument must be a multiple of 8 bits width" (actual_width 13)) |}];
  let bswap ~width x =
    print_s [%sexp (x, bswap (of_int ~width x) : Int.Hex.t * signal)]
  in
  bswap ~width:8 0xaa;
  [%expect {| (0xaa 8'b10101010) |}];
  bswap ~width:16 0x1122;
  [%expect {|
    (0x1122 16'h2211) |}];
  bswap ~width:24 0x123456;
  [%expect {| (0x123456 24'h563412) |}];
  bswap ~width:32 0xdeadbeef;
  [%expect {| (0xdeadbeef 32'hefbeadde) |}]
;;

let%expect_test "shifting" =
  print_s
    [%message
      "shifting"
        ~sll:(List.init 4 ~f:(fn2 sll (of_string "001")) : (signal, int) fn2 list)
        ~srl:(List.init 4 ~f:(fn2 srl (of_string "100")) : (signal, int) fn2 list)
        ~sra:(List.init 4 ~f:(fn2 sra (of_string "100")) : (signal, int) fn2 list)
        ~rotl:(List.init 4 ~f:(fn2 rotl (of_string "001")) : (signal, int) fn2 list)
        ~rotr:(List.init 4 ~f:(fn2 rotr (of_string "001")) : (signal, int) fn2 list)
        ~log_shift_sll:
          (List.init 4 ~f:(fun i ->
             fn2 (log_shift sll) (of_string "001") (of_int ~width:2 i))
           : (signal, signal) fn2 list)
        ~log_shift_srl:
          (List.init 4 ~f:(fun i ->
             fn2 (log_shift srl) (of_string "100") (of_int ~width:2 i))
           : (signal, signal) fn2 list)
        ~log_shift_sra:
          (List.init 4 ~f:(fun i ->
             fn2 (log_shift sra) (of_string "100") (of_int ~width:2 i))
           : (signal, signal) fn2 list)
        ~log_shift_rotl:
          (List.init 4 ~f:(fun i ->
             fn2 (log_shift rotl) (of_string "001") (of_int ~width:2 i))
           : (signal, signal) fn2 list)
        ~log_shift_rotr:
          (List.init 4 ~f:(fun i ->
             fn2 (log_shift rotr) (of_string "001") (of_int ~width:2 i))
           : (signal, signal) fn2 list)];
  [%expect
    {|
    (shifting
      (sll (
        ((3'b001 0) = 3'b001)
        ((3'b001 1) = 3'b010)
        ((3'b001 2) = 3'b100)
        ((3'b001 3) = 3'b000)))
      (srl (
        ((3'b100 0) = 3'b100)
        ((3'b100 1) = 3'b010)
        ((3'b100 2) = 3'b001)
        ((3'b100 3) = 3'b000)))
      (sra (
        ((3'b100 0) = 3'b100)
        ((3'b100 1) = 3'b110)
        ((3'b100 2) = 3'b111)
        ((3'b100 3) = 3'b111)))
      (rotl (
        ((3'b001 0) = 3'b001)
        ((3'b001 1) = 3'b010)
        ((3'b001 2) = 3'b100)
        ((3'b001 3) = 3'b001)))
      (rotr (
        ((3'b001 0) = 3'b001)
        ((3'b001 1) = 3'b100)
        ((3'b001 2) = 3'b010)
        ((3'b001 3) = 3'b001)))
      (log_shift_sll (
        ((3'b001 2'b00) = 3'b001)
        ((3'b001 2'b01) = 3'b010)
        ((3'b001 2'b10) = 3'b100)
        ((3'b001 2'b11) = 3'b000)))
      (log_shift_srl (
        ((3'b100 2'b00) = 3'b100)
        ((3'b100 2'b01) = 3'b010)
        ((3'b100 2'b10) = 3'b001)
        ((3'b100 2'b11) = 3'b000)))
      (log_shift_sra (
        ((3'b100 2'b00) = 3'b100)
        ((3'b100 2'b01) = 3'b110)
        ((3'b100 2'b10) = 3'b111)
        ((3'b100 2'b11) = 3'b111)))
      (log_shift_rotl (
        ((3'b001 2'b00) = 3'b001)
        ((3'b001 2'b01) = 3'b010)
        ((3'b001 2'b10) = 3'b100)
        ((3'b001 2'b11) = 3'b001)))
      (log_shift_rotr (
        ((3'b001 2'b00) = 3'b001)
        ((3'b001 2'b01) = 3'b100)
        ((3'b001 2'b10) = 3'b010)
        ((3'b001 2'b11) = 3'b001)))) |}]
;;

(* Various exceptions - this will be more exhaustively tested in the features that convert
   exceptions to sexps. *)

let%expect_test "add width exn" =
  require_does_raise [%here] (fun () -> of_int ~width:3 22 +: of_int ~width:8 33);
  [%expect
    {|
    ("[+:] got inputs of different widths" (
      (const (width 3) (value 0b110))
      (const (width 8) (value 0b00100001)))) |}]
;;

let%expect_test "sub width exn" =
  require_does_raise [%here] (fun () -> of_int ~width:3 22 -: of_int ~width:8 33);
  [%expect
    {|
    ("[-:] got inputs of different widths" (
      (const (width 3) (value 0b110))
      (const (width 8) (value 0b00100001)))) |}]
;;

let%expect_test "less than width exn" =
  require_does_raise [%here] (fun () -> of_string "01" <: of_string "001");
  [%expect
    {|
    ("[<:] got inputs of different widths" (
      (const (width 2) (value 0b01))
      (const (width 3) (value 0b001)))) |}]
;;

let%expect_test "greater than width exn" =
  require_does_raise [%here] (fun () -> of_string "01" >: of_string "001");
  [%expect
    {|
    ("[<:] got inputs of different widths" (
      (const (width 3) (value 0b001))
      (const (width 2) (value 0b01)))) |}]
;;

let%expect_test "less than or equal to width exn" =
  require_does_raise [%here] (fun () -> of_string "01" <=: of_string "001");
  [%expect
    {|
    ("[<:] got inputs of different widths" (
      (const (width 3) (value 0b001))
      (const (width 2) (value 0b01)))) |}]
;;

let%expect_test "greater than or equal to width exn" =
  require_does_raise [%here] (fun () -> of_string "01" >=: of_string "001");
  [%expect
    {|
    ("[<:] got inputs of different widths" (
      (const (width 2) (value 0b01))
      (const (width 3) (value 0b001)))) |}]
;;

let%expect_test "equals width exn" =
  require_does_raise [%here] (fun () -> of_string "01" ==: of_string "001");
  [%expect
    {|
    ("[==:] got inputs of different widths" (
      (const (width 2) (value 0b01))
      (const (width 3) (value 0b001)))) |}]
;;

let%expect_test "not equals width exn" =
  require_does_raise [%here] (fun () -> of_string "01" <>: of_string "001");
  [%expect
    {|
    ("[<>:] got inputs of different widths" (
      (const (width 2) (value 0b01))
      (const (width 3) (value 0b001)))) |}]
;;

let%expect_test "and width exn" =
  require_does_raise [%here] (fun () -> of_string "1010" &: of_string "100");
  [%expect
    {|
    ("[&:] got inputs of different widths" (
      (const (width 4) (value 0b1010))
      (const (width 3) (value 0b100)))) |}]
;;

let%expect_test "or width exn" =
  require_does_raise [%here] (fun () -> of_string "1010" |: of_string "100");
  [%expect
    {|
    ("[|:] got inputs of different widths" (
      (const (width 4) (value 0b1010))
      (const (width 3) (value 0b100)))) |}]
;;

let%expect_test "xor width exn" =
  require_does_raise [%here] (fun () -> of_string "1010" ^: of_string "100");
  [%expect
    {|
    ("[^:] got inputs of different widths" (
      (const (width 4) (value 0b1010))
      (const (width 3) (value 0b100)))) |}]
;;

let%expect_test "mux exn: idx too narrow" =
  let data4 = List.map ~f:(of_int ~width:5) [ 0; 10; 20; 30 ] in
  require_does_raise [%here] (fun () -> mux vdd data4);
  [%expect
    {|
    ("[mux] got too many inputs"
      (inputs_provided  4)
      (maximum_expected 2)) |}]
;;

let%expect_test "select out of bounds throws exn" =
  require_does_raise [%here] (fun () -> select vdd 1 1);
  [%expect
    {|
    ("[select] indices are out of bounds"
      (input_width 1)
      (hi          1)
      (lo          1)) |}]
;;

let%expect_test "select hi<lo throws exn" =
  require_does_raise [%here] (fun () -> select (of_int ~width:2 0) 0 1);
  [%expect {|
    ("[select] got [hi < lo]"
      (hi 0)
      (lo 1)) |}]
;;

let%expect_test "msbs exn" =
  require_does_raise [%here] (fun () -> msbs vdd);
  [%expect {|
    ("[select] got [hi < lo]"
      (hi 0)
      (lo 1)) |}]
;;

let%expect_test "lsbs exn" =
  require_does_raise [%here] (fun () -> lsbs vdd);
  [%expect {|
    ("[select] got [hi < lo]"
      (hi -1)
      (lo 0)) |}]
;;
