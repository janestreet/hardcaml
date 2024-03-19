open Base
open Hardcaml
open Signal
open Expect_test_helpers_base
open Hardcaml_test.Test_constant_propagation.Trace

let%expect_test "negate" =
  let negate = fn1 negate in
  print_s
    [%message
      "negate"
        ~_63_bit_min_max:
          ([ negate (of_int ~width:63 Int.max_value)
           ; negate (of_int ~width:63 Int.min_value)
           ; negate (of_int ~width:63 (Int.min_value + 1))
           ]
            : signal fn1 list)];
  [%expect
    {|
    (negate (
      _63_bit_min_max (
        (63'h3fffffffffffffff = 63'h4000000000000001)
        (63'h4000000000000000 = 63'h4000000000000000)
        (63'h4000000000000001 = 63'h3fffffffffffffff))))
    |}]
;;
