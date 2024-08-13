open Base
open Hardcaml
open Signal
open Expect_test_helpers_base
open Hardcaml_test.Test_conversion

let%expect_test "Bits.to_signed_int (min int)" =
  print_s
    [%message "" ~should_be_min_int:(Bits.to_signed_int Bits.(vdd @: zero 62) : int)];
  [%expect {| (should_be_min_int -4611686018427387904) |}];
  require_does_raise (fun () : int -> Bits.to_signed_int Bits.(vdd @: zero 63));
  [%expect {| "Failed to convert value to signed integer type" |}]
;;

let%expect_test "Bits.to_int (at msb)" =
  let convert_bits =
    convert_bits ~uint:to_unsigned_int ~sint:to_signed_int ~trunc:to_int_trunc
  in
  print_s
    [%message
      ""
        ~below_msb:
          (convert_bits ~f:(pad_zero 60) 2 : int Or_error.t signed_and_unsigned list)
        ~at_msb:
          (convert_bits ~f:(pad_zero 61) 2 : int Or_error.t signed_and_unsigned list)
        ~across_msb:
          (convert_bits ~f:(pad_zero 62) 2 : int Or_error.t signed_and_unsigned list)
        ~above_msb:
          (convert_bits ~f:(pad_zero 63) 2 : int Or_error.t signed_and_unsigned list)];
  [%expect
    {|
    ((below_msb (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1152921504606846976))
        (signed    (Ok 1152921504606846976))
        (truncated (Ok 1152921504606846976)))
       ((unsigned  (Ok 2305843009213693952))
        (signed    (Ok -2305843009213693952))
        (truncated (Ok 2305843009213693952)))
       ((unsigned  (Ok 3458764513820540928))
        (signed    (Ok -1152921504606846976))
        (truncated (Ok 3458764513820540928)))))
     (at_msb (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 2305843009213693952))
        (signed    (Ok 2305843009213693952))
        (truncated (Ok 2305843009213693952)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -4611686018427387904))
        (truncated (Ok    -4611686018427387904)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -2305843009213693952))
        (truncated (Ok    -2305843009213693952)))))
     (across_msb (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Error "conversion failed"))
        (truncated (Ok    -4611686018427387904)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Error "conversion failed"))
        (truncated (Ok    0)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -4611686018427387904))
        (truncated (Ok    -4611686018427387904)))))
     (above_msb (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Error "conversion failed"))
        (truncated (Ok    0)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Error "conversion failed"))
        (truncated (Ok    0)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Error "conversion failed"))
        (truncated (Ok    0))))))
    |}]
;;
