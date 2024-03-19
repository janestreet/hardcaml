open Base
open Hardcaml
open Signal
open Expect_test_helpers_base
open Hardcaml_test.Test_conversion

let%expect_test "Bits.to_sint (min int)" =
  print_s
    [%message
      ""
        ~should_be_min_int:(Bits.to_sint Bits.(vdd @: zero 62) : int)
        ~should_be_0:(Bits.to_sint Bits.(vdd @: zero 63) : int)];
  [%expect
    {|
    ((should_be_min_int -4611686018427387904)
     (should_be_0       0))
    |}]
;;

let%expect_test "Bits.to_[s]int (at msb)" =
  let convert_bits = convert_bits ~uint:to_int ~sint:to_sint in
  print_s
    [%message
      ""
        ~at_msb:(convert_bits ~f:(pad_zero 61) 2 : int signed_and_unsigned list)
        ~across_msb:(convert_bits ~f:(pad_zero 62) 2 : int signed_and_unsigned list)
        ~above_msb:(convert_bits ~f:(pad_zero 63) 2 : int signed_and_unsigned list)];
  [%expect
    {|
    ((at_msb (
       ((unsigned 0)
        (signed   0))
       ((unsigned 2305843009213693952)
        (signed   2305843009213693952))
       ((unsigned -4611686018427387904)
        (signed   -4611686018427387904))
       ((unsigned -2305843009213693952)
        (signed   -2305843009213693952))))
     (across_msb (
       ((unsigned 0)
        (signed   0))
       ((unsigned -4611686018427387904)
        (signed   -4611686018427387904))
       ((unsigned 0)
        (signed   0))
       ((unsigned -4611686018427387904)
        (signed   -4611686018427387904))))
     (above_msb (
       ((unsigned 0) (signed 0))
       ((unsigned 0) (signed 0))
       ((unsigned 0) (signed 0))
       ((unsigned 0) (signed 0)))))
    |}]
;;
