open! Import
open Signal

type 'a signed_and_unsigned =
  { unsigned : 'a
  ; signed : 'a
  }
[@@deriving sexp_of]

let bits ~f bits = List.init (1 lsl bits) ~f:(fun i -> of_int ~width:bits i |> f)
let convert ~uint ~sint const = { signed = sint const; unsigned = uint const }

let convert_bits ?(f = Fn.id) ~uint ~sint b =
  List.map ~f:(convert ~uint ~sint) (bits ~f b)
;;

let pad_zero n c = c @: zero n

let%expect_test "Bits.to_sint" =
  print_s
    [%message
      ""
        ~should_be_minus_1:(Bits.to_sint (Bits.of_bit_string "1") : int)
        ~should_be_minus_2:(Bits.to_sint (Bits.of_bit_string "10") : int)
        ~should_be_min_int:(Bits.to_sint Bits.(vdd @: zero 62) : int)
        ~should_be_:(Bits.to_sint Bits.(vdd @: zero 63) : int)];
  [%expect
    {|
    ((should_be_minus_1 -1)
     (should_be_minus_2 -2)
     (should_be_min_int -4611686018427387904)
     (should_be_        0)) |}]
;;

let%expect_test "to_[s]int" =
  let convert_bits = convert_bits ~uint:to_int ~sint:to_sint in
  print_s
    [%message
      ""
        ~bits_1:(convert_bits 1 : int signed_and_unsigned list)
        ~bits_2:(convert_bits 2 : int signed_and_unsigned list)
        ~bits_3:(convert_bits 3 : int signed_and_unsigned list)
        ~bits_4:(convert_bits 4 : int signed_and_unsigned list)
        ~at_msb:(convert_bits ~f:(pad_zero 61) 2 : int signed_and_unsigned list)
        ~across_msb:(convert_bits ~f:(pad_zero 62) 2 : int signed_and_unsigned list)
        ~above_msb:(convert_bits ~f:(pad_zero 63) 2 : int signed_and_unsigned list)];
  [%expect
    {|
    ((bits_1 (
       ((unsigned 0) (signed 0))
       ((unsigned 1) (signed -1))))
     (bits_2 (
       ((unsigned 0) (signed 0))
       ((unsigned 1) (signed 1))
       ((unsigned 2) (signed -2))
       ((unsigned 3) (signed -1))))
     (bits_3 (
       ((unsigned 0) (signed 0))
       ((unsigned 1) (signed 1))
       ((unsigned 2) (signed 2))
       ((unsigned 3) (signed 3))
       ((unsigned 4) (signed -4))
       ((unsigned 5) (signed -3))
       ((unsigned 6) (signed -2))
       ((unsigned 7) (signed -1))))
     (bits_4 (
       ((unsigned 0)  (signed 0))
       ((unsigned 1)  (signed 1))
       ((unsigned 2)  (signed 2))
       ((unsigned 3)  (signed 3))
       ((unsigned 4)  (signed 4))
       ((unsigned 5)  (signed 5))
       ((unsigned 6)  (signed 6))
       ((unsigned 7)  (signed 7))
       ((unsigned 8)  (signed -8))
       ((unsigned 9)  (signed -7))
       ((unsigned 10) (signed -6))
       ((unsigned 11) (signed -5))
       ((unsigned 12) (signed -4))
       ((unsigned 13) (signed -3))
       ((unsigned 14) (signed -2))
       ((unsigned 15) (signed -1))))
     (at_msb (
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
       ((unsigned 0) (signed 0))))) |}]
;;

let%expect_test "to_[s]int32" =
  let convert_bits = convert_bits ~uint:to_int32 ~sint:to_sint32 in
  print_s
    [%message
      ""
        ~bits_1:(convert_bits 1 : int32 signed_and_unsigned list)
        ~bits_2:(convert_bits 2 : int32 signed_and_unsigned list)
        ~bits_3:(convert_bits 3 : int32 signed_and_unsigned list)
        ~bits_4:(convert_bits 4 : int32 signed_and_unsigned list)
        ~at_msb:(convert_bits ~f:(pad_zero 30) 2 : int32 signed_and_unsigned list)
        ~across_msb:(convert_bits ~f:(pad_zero 61) 2 : int32 signed_and_unsigned list)
        ~above_msb:(convert_bits ~f:(pad_zero 32) 2 : int32 signed_and_unsigned list)];
  [%expect
    {|
   ((bits_1 (
      ((unsigned 0) (signed 0))
      ((unsigned 1) (signed -1))))
    (bits_2 (
      ((unsigned 0) (signed 0))
      ((unsigned 1) (signed 1))
      ((unsigned 2) (signed -2))
      ((unsigned 3) (signed -1))))
    (bits_3 (
      ((unsigned 0) (signed 0))
      ((unsigned 1) (signed 1))
      ((unsigned 2) (signed 2))
      ((unsigned 3) (signed 3))
      ((unsigned 4) (signed -4))
      ((unsigned 5) (signed -3))
      ((unsigned 6) (signed -2))
      ((unsigned 7) (signed -1))))
    (bits_4 (
      ((unsigned 0)  (signed 0))
      ((unsigned 1)  (signed 1))
      ((unsigned 2)  (signed 2))
      ((unsigned 3)  (signed 3))
      ((unsigned 4)  (signed 4))
      ((unsigned 5)  (signed 5))
      ((unsigned 6)  (signed 6))
      ((unsigned 7)  (signed 7))
      ((unsigned 8)  (signed -8))
      ((unsigned 9)  (signed -7))
      ((unsigned 10) (signed -6))
      ((unsigned 11) (signed -5))
      ((unsigned 12) (signed -4))
      ((unsigned 13) (signed -3))
      ((unsigned 14) (signed -2))
      ((unsigned 15) (signed -1))))
    (at_msb (
      ((unsigned 0)           (signed 0))
      ((unsigned 1073741824)  (signed 1073741824))
      ((unsigned -2147483648) (signed -2147483648))
      ((unsigned -1073741824) (signed -1073741824))))
    (across_msb (
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))))
    (above_msb (
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))))) |}]
;;

let%expect_test "to_[s]int64" =
  let convert_bits = convert_bits ~uint:to_int64 ~sint:to_sint64 in
  print_s
    [%message
      ""
        ~bits_1:(convert_bits 1 : int64 signed_and_unsigned list)
        ~bits_2:(convert_bits 2 : int64 signed_and_unsigned list)
        ~bits_3:(convert_bits 3 : int64 signed_and_unsigned list)
        ~bits_4:(convert_bits 4 : int64 signed_and_unsigned list)
        ~at_msb:(convert_bits ~f:(pad_zero 62) 2 : int64 signed_and_unsigned list)
        ~across_msb:(convert_bits ~f:(pad_zero 63) 2 : int64 signed_and_unsigned list)
        ~above_msb:(convert_bits ~f:(pad_zero 64) 2 : int64 signed_and_unsigned list)];
  [%expect
    {|
   ((bits_1 (
      ((unsigned 0) (signed 0))
      ((unsigned 1) (signed -1))))
    (bits_2 (
      ((unsigned 0) (signed 0))
      ((unsigned 1) (signed 1))
      ((unsigned 2) (signed -2))
      ((unsigned 3) (signed -1))))
    (bits_3 (
      ((unsigned 0) (signed 0))
      ((unsigned 1) (signed 1))
      ((unsigned 2) (signed 2))
      ((unsigned 3) (signed 3))
      ((unsigned 4) (signed -4))
      ((unsigned 5) (signed -3))
      ((unsigned 6) (signed -2))
      ((unsigned 7) (signed -1))))
    (bits_4 (
      ((unsigned 0)  (signed 0))
      ((unsigned 1)  (signed 1))
      ((unsigned 2)  (signed 2))
      ((unsigned 3)  (signed 3))
      ((unsigned 4)  (signed 4))
      ((unsigned 5)  (signed 5))
      ((unsigned 6)  (signed 6))
      ((unsigned 7)  (signed 7))
      ((unsigned 8)  (signed -8))
      ((unsigned 9)  (signed -7))
      ((unsigned 10) (signed -6))
      ((unsigned 11) (signed -5))
      ((unsigned 12) (signed -4))
      ((unsigned 13) (signed -3))
      ((unsigned 14) (signed -2))
      ((unsigned 15) (signed -1))))
    (at_msb (
      ((unsigned 0)
       (signed   0))
      ((unsigned 4611686018427387904)
       (signed   4611686018427387904))
      ((unsigned -9223372036854775808)
       (signed   -9223372036854775808))
      ((unsigned -4611686018427387904)
       (signed   -4611686018427387904))))
    (across_msb (
      ((unsigned 0)
       (signed   0))
      ((unsigned -9223372036854775808)
       (signed   -9223372036854775808))
      ((unsigned 0)
       (signed   0))
      ((unsigned -9223372036854775808)
       (signed   -9223372036854775808))))
    (above_msb (
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))
      ((unsigned 0) (signed 0))))) |}]
;;

let%expect_test "to_bstr" =
  print_s
    [%sexp
      (List.map
         ~f:(fun c -> of_string c |> to_bstr)
         [ "0"; "1"; "0000"; "1111"; "10011"; "1101000101010101011010" ]
       : string list)];
  [%expect {| (0 1 0000 1111 10011 1101000101010101011010) |}]
;;
