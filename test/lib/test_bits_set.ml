open! Import
open Bits

let all width ~f =
  for i = 0 to (1 lsl width) - 1 do
    let b = Bits.of_int_trunc ~width i in
    let result = f b in
    printf "%s: %s\n" (to_bstr b) (to_bstr result)
  done
;;

let%expect_test "any bit set" =
  all 1 ~f:any_bit_set;
  [%expect
    {|
    0: 0
    1: 1
    |}];
  all 2 ~f:any_bit_set;
  [%expect
    {|
    00: 0
    01: 1
    10: 1
    11: 1
    |}];
  all 3 ~f:any_bit_set;
  [%expect
    {|
    000: 0
    001: 1
    010: 1
    011: 1
    100: 1
    101: 1
    110: 1
    111: 1
    |}]
;;

let%expect_test "all bits set" =
  all 1 ~f:all_bits_set;
  [%expect
    {|
    0: 0
    1: 1
    |}];
  all 2 ~f:all_bits_set;
  [%expect
    {|
    00: 0
    01: 0
    10: 0
    11: 1
    |}];
  all 3 ~f:all_bits_set;
  [%expect
    {|
    000: 0
    001: 0
    010: 0
    011: 0
    100: 0
    101: 0
    110: 0
    111: 1
    |}]
;;

let%expect_test "no bits set" =
  all 1 ~f:no_bits_set;
  [%expect
    {|
    0: 1
    1: 0
    |}];
  all 2 ~f:no_bits_set;
  [%expect
    {|
    00: 1
    01: 0
    10: 0
    11: 0
    |}];
  all 3 ~f:no_bits_set;
  [%expect
    {|
    000: 1
    001: 0
    010: 0
    011: 0
    100: 0
    101: 0
    110: 0
    111: 0
    |}]
;;
