open! Import
open Signal
open Test_constant_propagation.Trace

let%expect_test "add" =
  print_s @@ binary_op_tests "+:" ( +: ) ( +:. );
  [%expect
    {|
    (+:
      (all_1_bit (
        (1'b0 +: 1'b0 = 1'b0)
        (1'b0 +: 1'b1 = 1'b1)
        (1'b1 +: 1'b0 = 1'b1)
        (1'b1 +: 1'b1 = 1'b0)))
      (all_2_bits (
        (2'b00 +: 2'b00 = 2'b00)
        (2'b00 +: 2'b01 = 2'b01)
        (2'b00 +: 2'b10 = 2'b10)
        (2'b00 +: 2'b11 = 2'b11)
        (2'b01 +: 2'b00 = 2'b01)
        (2'b01 +: 2'b01 = 2'b10)
        (2'b01 +: 2'b10 = 2'b11)
        (2'b01 +: 2'b11 = 2'b00)
        (2'b10 +: 2'b00 = 2'b10)
        (2'b10 +: 2'b01 = 2'b11)
        (2'b10 +: 2'b10 = 2'b00)
        (2'b10 +: 2'b11 = 2'b01)
        (2'b11 +: 2'b00 = 2'b11)
        (2'b11 +: 2'b01 = 2'b00)
        (2'b11 +: 2'b10 = 2'b01)
        (2'b11 +: 2'b11 = 2'b10)))
      (misc (
        (8'b00010110 +: 8'b00100001 = 8'b00110111)
        (123'h0000000000000000000000000005749
         +:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         123'h0000000000000000000000000000000)))
      (int_on_right (
        (7'b0011011 +: 12  = 7'b0100111)
        (7'b0011011 +: -12 = 7'b0001111)))) |}]
;;

let%expect_test "[+:] with one constant" =
  binary_op_tests_with_one_constant "+:" ( +: );
  [%expect
    {|
    (1'b0 +: x = x)
    (x +: 1'b0 = x)
    (1'b1
     +:
     x
     =
     ("Not a constant" (
       signal (
         add
         (width 1)
         (arguments (
           (const
             (width 1)
             (value 0b1))
           (wire
             (names (x))
             (width   1)
             (data_in empty))))))))
    (x
     +:
     1'b1
     =
     ("Not a constant" (
       signal (
         add
         (width 1)
         (arguments (
           (wire
             (names (x))
             (width   1)
             (data_in empty))
           (const
             (width 1)
             (value 0b1))))))))
    (2'b00 +: x = x)
    (x +: 2'b00 = x)
    (2'b01
     +:
     x
     =
     ("Not a constant" (
       signal (
         add
         (width 2)
         (arguments (
           (const
             (width 2)
             (value 0b01))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     +:
     2'b01
     =
     ("Not a constant" (
       signal (
         add
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b01))))))))
    (2'b10
     +:
     x
     =
     ("Not a constant" (
       signal (
         add
         (width 2)
         (arguments (
           (const
             (width 2)
             (value 0b10))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     +:
     2'b10
     =
     ("Not a constant" (
       signal (
         add
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b10))))))))
    (2'b11
     +:
     x
     =
     ("Not a constant" (
       signal (
         add
         (width 2)
         (arguments (
           (const
             (width 2)
             (value 0b11))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     +:
     2'b11
     =
     ("Not a constant" (
       signal (
         add
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b11)))))))) |}]
;;

let%expect_test "sub" =
  print_s @@ binary_op_tests "-:" ( -: ) ( -:. );
  [%expect
    {|
    (-:
      (all_1_bit (
        (1'b0 -: 1'b0 = 1'b0)
        (1'b0 -: 1'b1 = 1'b1)
        (1'b1 -: 1'b0 = 1'b1)
        (1'b1 -: 1'b1 = 1'b0)))
      (all_2_bits (
        (2'b00 -: 2'b00 = 2'b00)
        (2'b00 -: 2'b01 = 2'b11)
        (2'b00 -: 2'b10 = 2'b10)
        (2'b00 -: 2'b11 = 2'b01)
        (2'b01 -: 2'b00 = 2'b01)
        (2'b01 -: 2'b01 = 2'b00)
        (2'b01 -: 2'b10 = 2'b11)
        (2'b01 -: 2'b11 = 2'b10)
        (2'b10 -: 2'b00 = 2'b10)
        (2'b10 -: 2'b01 = 2'b01)
        (2'b10 -: 2'b10 = 2'b00)
        (2'b10 -: 2'b11 = 2'b11)
        (2'b11 -: 2'b00 = 2'b11)
        (2'b11 -: 2'b01 = 2'b10)
        (2'b11 -: 2'b10 = 2'b01)
        (2'b11 -: 2'b11 = 2'b00)))
      (misc (
        (8'b00010110 -: 8'b00100001 = 8'b11110101)
        (123'h0000000000000000000000000005749
         -:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         123'h000000000000000000000000000ae92)))
      (int_on_right (
        (7'b0011011 -: 12  = 7'b0001111)
        (7'b0011011 -: -12 = 7'b0100111)))) |}]
;;

let%expect_test "[-:] with one constant" =
  binary_op_tests_with_one_constant "-:" ( -: );
  [%expect
    {|
    (1'b0
     -:
     x
     =
     ("Not a constant" (
       signal (
         sub
         (width 1)
         (arguments (
           (const
             (width 1)
             (value 0b0))
           (wire
             (names (x))
             (width   1)
             (data_in empty))))))))
    (x -: 1'b0 = x)
    (1'b1
     -:
     x
     =
     ("Not a constant" (
       signal (
         sub
         (width 1)
         (arguments (
           (const
             (width 1)
             (value 0b1))
           (wire
             (names (x))
             (width   1)
             (data_in empty))))))))
    (x
     -:
     1'b1
     =
     ("Not a constant" (
       signal (
         sub
         (width 1)
         (arguments (
           (wire
             (names (x))
             (width   1)
             (data_in empty))
           (const
             (width 1)
             (value 0b1))))))))
    (2'b00
     -:
     x
     =
     ("Not a constant" (
       signal (
         sub
         (width 2)
         (arguments (
           (const
             (width 2)
             (value 0b00))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x -: 2'b00 = x)
    (2'b01
     -:
     x
     =
     ("Not a constant" (
       signal (
         sub
         (width 2)
         (arguments (
           (const
             (width 2)
             (value 0b01))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     -:
     2'b01
     =
     ("Not a constant" (
       signal (
         sub
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b01))))))))
    (2'b10
     -:
     x
     =
     ("Not a constant" (
       signal (
         sub
         (width 2)
         (arguments (
           (const
             (width 2)
             (value 0b10))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     -:
     2'b10
     =
     ("Not a constant" (
       signal (
         sub
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b10))))))))
    (2'b11
     -:
     x
     =
     ("Not a constant" (
       signal (
         sub
         (width 2)
         (arguments (
           (const
             (width 2)
             (value 0b11))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     -:
     2'b11
     =
     ("Not a constant" (
       signal (
         sub
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b11)))))))) |}]
;;

let%expect_test "multiplication" =
  print_s
  @@ [%message
    "" ~_:(mul_op_tests "*:" ( *: ) : Sexp.t) ~_:(mul_op_tests "*+" ( *+ ) : Sexp.t)];
  [%expect
    {|
    ((*:
       (all_1_bit (
         (1'b0 *: 1'b0 = 2'b00)
         (1'b0 *: 1'b1 = 2'b00)
         (1'b1 *: 1'b0 = 2'b00)
         (1'b1 *: 1'b1 = 2'b01)))
       (all_2_bits (
         (2'b00 *: 2'b00 = 4'b0000)
         (2'b00 *: 2'b01 = 4'b0000)
         (2'b00 *: 2'b10 = 4'b0000)
         (2'b00 *: 2'b11 = 4'b0000)
         (2'b01 *: 2'b00 = 4'b0000)
         (2'b01 *: 2'b01 = 4'b0001)
         (2'b01 *: 2'b10 = 4'b0010)
         (2'b01 *: 2'b11 = 4'b0011)
         (2'b10 *: 2'b00 = 4'b0000)
         (2'b10 *: 2'b01 = 4'b0010)
         (2'b10 *: 2'b10 = 4'b0100)
         (2'b10 *: 2'b11 = 4'b0110)
         (2'b11 *: 2'b00 = 4'b0000)
         (2'b11 *: 2'b01 = 4'b0011)
         (2'b11 *: 2'b10 = 4'b0110)
         (2'b11 *: 2'b11 = 4'b1001)))
       (pow2 (
         (2'b11 *: 5'b00001 = 7'b0000011)
         (2'b11 *: 5'b00010 = 7'b0000110)
         (2'b11 *: 5'b00100 = 7'b0001100)
         (2'b11 *: 5'b01000 = 7'b0011000)
         (2'b11 *: 5'b10000 = 7'b0110000)))
       (misc (
         (8'b00010110 *: 8'b00100001 = 16'h02d6)
         (7'b0011011  *: 4'b1100     = 11'h144))))
     (*+
       (all_1_bit (
         (1'b0 *+ 1'b0 = 2'b00)
         (1'b0 *+ 1'b1 = 2'b00)
         (1'b1 *+ 1'b0 = 2'b00)
         (1'b1 *+ 1'b1 = 2'b01)))
       (all_2_bits (
         (2'b00 *+ 2'b00 = 4'b0000)
         (2'b00 *+ 2'b01 = 4'b0000)
         (2'b00 *+ 2'b10 = 4'b0000)
         (2'b00 *+ 2'b11 = 4'b0000)
         (2'b01 *+ 2'b00 = 4'b0000)
         (2'b01 *+ 2'b01 = 4'b0001)
         (2'b01 *+ 2'b10 = 4'b1110)
         (2'b01 *+ 2'b11 = 4'b1111)
         (2'b10 *+ 2'b00 = 4'b0000)
         (2'b10 *+ 2'b01 = 4'b1110)
         (2'b10 *+ 2'b10 = 4'b0100)
         (2'b10 *+ 2'b11 = 4'b0010)
         (2'b11 *+ 2'b00 = 4'b0000)
         (2'b11 *+ 2'b01 = 4'b1111)
         (2'b11 *+ 2'b10 = 4'b0010)
         (2'b11 *+ 2'b11 = 4'b0001)))
       (pow2 (
         (2'b11 *+ 5'b00001 = 7'b1111111)
         (2'b11 *+ 5'b00010 = 7'b1111110)
         (2'b11 *+ 5'b00100 = 7'b1111100)
         (2'b11 *+ 5'b01000 = 7'b1111000)
         (2'b11 *+ 5'b10000 = 7'b0010000)))
       (misc (
         (8'b00010110 *+ 8'b00100001 = 16'h02d6)
         (7'b0011011  *+ 4'b1100     = 11'h794))))) |}]
;;

let%expect_test "multiplication with one constant" =
  binary_op_tests_with_one_constant "*:" ( *: );
  [%expect
    {|
    (1'b0 *: x = 2'b00)
    (x *: 1'b0 = 2'b00)
    (1'b1
     *:
     x
     =
     ("Not a constant" (
       signal (
         cat
         (width 2)
         (arguments (
           (const
             (width 1)
             (value 0b0))
           (wire
             (names (x))
             (width   1)
             (data_in empty))))))))
    (x
     *:
     1'b1
     =
     ("Not a constant" (
       signal (
         cat
         (width 2)
         (arguments (
           (const
             (width 1)
             (value 0b0))
           (wire
             (names (x))
             (width   1)
             (data_in empty))))))))
    (2'b00 *: x = 4'b0000)
    (x *: 2'b00 = 4'b0000)
    (2'b01
     *:
     x
     =
     ("Not a constant" (
       signal (
         cat
         (width 4)
         (arguments (
           (const
             (width 2)
             (value 0b00))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     *:
     2'b01
     =
     ("Not a constant" (
       signal (
         cat
         (width 4)
         (arguments (
           (const
             (width 2)
             (value 0b00))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (2'b10
     *:
     x
     =
     ("Not a constant" (
       signal (
         cat
         (width 4)
         (arguments (
           (const
             (names (gnd))
             (width 1)
             (value 0b0))
           (cat
             (width 3)
             (arguments (
               (wire
                 (names (x))
                 (width   2)
                 (data_in empty))
               (const
                 (width 1)
                 (value 0b0)))))))))))
    (x
     *:
     2'b10
     =
     ("Not a constant" (
       signal (
         cat
         (width 4)
         (arguments (
           (const
             (names (gnd))
             (width 1)
             (value 0b0))
           (cat
             (width 3)
             (arguments (
               (wire
                 (names (x))
                 (width   2)
                 (data_in empty))
               (const
                 (width 1)
                 (value 0b0)))))))))))
    (2'b11
     *:
     x
     =
     ("Not a constant" (
       signal (
         mulu
         (width 4)
         (arguments (
           (const
             (width 2)
             (value 0b11))
           (wire
             (names (x))
             (width   2)
             (data_in empty))))))))
    (x
     *:
     2'b11
     =
     ("Not a constant" (
       signal (
         mulu
         (width 4)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b11)))))))) |}]
;;

let%expect_test "negate" =
  let negate = fn1 negate in
  print_s
    [%message
      "negate"
        ~all_1_bit:(op1 1 ~f:negate : signal fn1 list)
        ~all_2_bits:(op1 2 ~f:negate : signal fn1 list)
        ~all_3_bits:(op1 3 ~f:negate : signal fn1 list)
        ~_63_bit_min_max:
          ([ negate (of_int ~width:63 Int.max_value)
           ; negate (of_int ~width:63 Int.min_value)
           ; negate (of_int ~width:63 (Int.min_value + 1))
           ]
           : signal fn1 list)];
  [%expect
    {|
    (negate
      (all_1_bit (
        (1'b0 = 1'b0)
        (1'b1 = 1'b1)))
      (all_2_bits (
        (2'b00 = 2'b00)
        (2'b01 = 2'b11)
        (2'b10 = 2'b10)
        (2'b11 = 2'b01)))
      (all_3_bits (
        (3'b000 = 3'b000)
        (3'b001 = 3'b111)
        (3'b010 = 3'b110)
        (3'b011 = 3'b101)
        (3'b100 = 3'b100)
        (3'b101 = 3'b011)
        (3'b110 = 3'b010)
        (3'b111 = 3'b001)))
      (_63_bit_min_max (
        (63'h3fffffffffffffff = 63'h4000000000000001)
        (63'h4000000000000000 = 63'h4000000000000000)
        (63'h4000000000000001 = 63'h3fffffffffffffff)))) |}]
;;
