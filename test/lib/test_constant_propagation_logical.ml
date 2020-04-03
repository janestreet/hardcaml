open! Import
open Signal
open Test_constant_propagation.Trace

let%expect_test "and" =
  print_s @@ binary_op_tests "&:" ( &: ) ( &:. );
  [%expect
    {|
    (&:
      (all_1_bit (
        (1'b0 &: 1'b0 = 1'b0)
        (1'b0 &: 1'b1 = 1'b0)
        (1'b1 &: 1'b0 = 1'b0)
        (1'b1 &: 1'b1 = 1'b1)))
      (all_2_bits (
        (2'b00 &: 2'b00 = 2'b00)
        (2'b00 &: 2'b01 = 2'b00)
        (2'b00 &: 2'b10 = 2'b00)
        (2'b00 &: 2'b11 = 2'b00)
        (2'b01 &: 2'b00 = 2'b00)
        (2'b01 &: 2'b01 = 2'b01)
        (2'b01 &: 2'b10 = 2'b00)
        (2'b01 &: 2'b11 = 2'b01)
        (2'b10 &: 2'b00 = 2'b00)
        (2'b10 &: 2'b01 = 2'b00)
        (2'b10 &: 2'b10 = 2'b10)
        (2'b10 &: 2'b11 = 2'b10)
        (2'b11 &: 2'b00 = 2'b00)
        (2'b11 &: 2'b01 = 2'b01)
        (2'b11 &: 2'b10 = 2'b10)
        (2'b11 &: 2'b11 = 2'b11)))
      (misc (
        (8'b00010110 &: 8'b00100001 = 8'b00000000)
        (123'h0000000000000000000000000005749
         &:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         123'h0000000000000000000000000000001)))
      (int_on_right (
        (7'b0011011 &: 12  = 7'b0001000)
        (7'b0011011 &: -12 = 7'b0010000)))) |}]
;;

let%expect_test "[&:] with one constant" =
  binary_op_tests_with_one_constant "&:" ( &: );
  [%expect
    {|
    (1'b0 &: x = 1'b0)
    (x &: 1'b0 = 1'b0)
    (1'b1 &: x = x)
    (x &: 1'b1 = x)
    (2'b00 &: x = 2'b00)
    (x &: 2'b00 = 2'b00)
    (2'b01
     &:
     x
     =
     ("Not a constant" (
       signal (
         and
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
     &:
     2'b01
     =
     ("Not a constant" (
       signal (
         and
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
     &:
     x
     =
     ("Not a constant" (
       signal (
         and
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
     &:
     2'b10
     =
     ("Not a constant" (
       signal (
         and
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b10))))))))
    (2'b11 &: x = x)
    (x &: 2'b11 = x) |}]
;;

let%expect_test "or" =
  print_s @@ binary_op_tests "|:" ( |: ) ( |:. );
  [%expect
    {|
    (|:
      (all_1_bit (
        (1'b0 |: 1'b0 = 1'b0)
        (1'b0 |: 1'b1 = 1'b1)
        (1'b1 |: 1'b0 = 1'b1)
        (1'b1 |: 1'b1 = 1'b1)))
      (all_2_bits (
        (2'b00 |: 2'b00 = 2'b00)
        (2'b00 |: 2'b01 = 2'b01)
        (2'b00 |: 2'b10 = 2'b10)
        (2'b00 |: 2'b11 = 2'b11)
        (2'b01 |: 2'b00 = 2'b01)
        (2'b01 |: 2'b01 = 2'b01)
        (2'b01 |: 2'b10 = 2'b11)
        (2'b01 |: 2'b11 = 2'b11)
        (2'b10 |: 2'b00 = 2'b10)
        (2'b10 |: 2'b01 = 2'b11)
        (2'b10 |: 2'b10 = 2'b10)
        (2'b10 |: 2'b11 = 2'b11)
        (2'b11 |: 2'b00 = 2'b11)
        (2'b11 |: 2'b01 = 2'b11)
        (2'b11 |: 2'b10 = 2'b11)
        (2'b11 |: 2'b11 = 2'b11)))
      (misc (
        (8'b00010110 |: 8'b00100001 = 8'b00110111)
        (123'h0000000000000000000000000005749
         |:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         123'h7ffffffffffffffffffffffffffffff)))
      (int_on_right (
        (7'b0011011 |: 12  = 7'b0011111)
        (7'b0011011 |: -12 = 7'b1111111)))) |}]
;;

let%expect_test "[|:] with one constant" =
  binary_op_tests_with_one_constant "|:" ( |: );
  [%expect
    {|
    (1'b0 |: x = x)
    (x |: 1'b0 = x)
    (1'b1 |: x = 1'b1)
    (x |: 1'b1 = 1'b1)
    (2'b00 |: x = x)
    (x |: 2'b00 = x)
    (2'b01
     |:
     x
     =
     ("Not a constant" (
       signal (
         or
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
     |:
     2'b01
     =
     ("Not a constant" (
       signal (
         or
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
     |:
     x
     =
     ("Not a constant" (
       signal (
         or
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
     |:
     2'b10
     =
     ("Not a constant" (
       signal (
         or
         (width 2)
         (arguments (
           (wire
             (names (x))
             (width   2)
             (data_in empty))
           (const
             (width 2)
             (value 0b10))))))))
    (2'b11 |: x = 2'b11)
    (x |: 2'b11 = 2'b11) |}]
;;

let%expect_test "xor" =
  print_s @@ binary_op_tests "^:" ( ^: ) ( ^:. );
  [%expect
    {|
    (^:
      (all_1_bit (
        (1'b0 ^: 1'b0 = 1'b0)
        (1'b0 ^: 1'b1 = 1'b1)
        (1'b1 ^: 1'b0 = 1'b1)
        (1'b1 ^: 1'b1 = 1'b0)))
      (all_2_bits (
        (2'b00 ^: 2'b00 = 2'b00)
        (2'b00 ^: 2'b01 = 2'b01)
        (2'b00 ^: 2'b10 = 2'b10)
        (2'b00 ^: 2'b11 = 2'b11)
        (2'b01 ^: 2'b00 = 2'b01)
        (2'b01 ^: 2'b01 = 2'b00)
        (2'b01 ^: 2'b10 = 2'b11)
        (2'b01 ^: 2'b11 = 2'b10)
        (2'b10 ^: 2'b00 = 2'b10)
        (2'b10 ^: 2'b01 = 2'b11)
        (2'b10 ^: 2'b10 = 2'b00)
        (2'b10 ^: 2'b11 = 2'b01)
        (2'b11 ^: 2'b00 = 2'b11)
        (2'b11 ^: 2'b01 = 2'b10)
        (2'b11 ^: 2'b10 = 2'b01)
        (2'b11 ^: 2'b11 = 2'b00)))
      (misc (
        (8'b00010110 ^: 8'b00100001 = 8'b00110111)
        (123'h0000000000000000000000000005749
         ^:
         123'h7ffffffffffffffffffffffffffa8b7
         =
         123'h7fffffffffffffffffffffffffffffe)))
      (int_on_right (
        (7'b0011011 ^: 12  = 7'b0010111)
        (7'b0011011 ^: -12 = 7'b1101111)))) |}]
;;

let%expect_test "not" =
  let ( ~: ) = fn1 ( ~: ) in
  print_s
    [%message
      "~:"
        ~all_1_bit:(op1 1 ~f:( ~: ) : signal fn1 list)
        ~all_2_bits:(op1 2 ~f:( ~: ) : signal fn1 list)
        ~all_3_bits:(op1 3 ~f:( ~: ) : signal fn1 list)];
  [%expect
    {|
    (~:
      (all_1_bit (
        (1'b0 = 1'b1)
        (1'b1 = 1'b0)))
      (all_2_bits (
        (2'b00 = 2'b11)
        (2'b01 = 2'b10)
        (2'b10 = 2'b01)
        (2'b11 = 2'b00)))
      (all_3_bits (
        (3'b000 = 3'b111)
        (3'b001 = 3'b110)
        (3'b010 = 3'b101)
        (3'b011 = 3'b100)
        (3'b100 = 3'b011)
        (3'b101 = 3'b010)
        (3'b110 = 3'b001)
        (3'b111 = 3'b000)))) |}]
;;
