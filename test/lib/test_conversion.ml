open! Import
open Signal

type 'a signed_and_unsigned =
  { unsigned : 'a
  ; signed : 'a
  ; truncated : 'a
  }
[@@deriving sexp_of]

let bits ~f bits = List.init (1 lsl bits) ~f:(fun i -> of_int_trunc ~width:bits i |> f)

let convert ~uint ~sint ~trunc const =
  let tryf f =
    try Ok (f const) with
    | _ -> Or_error.error_s [%message "conversion failed"]
  in
  { signed = tryf sint; unsigned = tryf uint; truncated = tryf trunc }
;;

let convert_bits ?(f = Fn.id) ~uint ~sint ~trunc b =
  List.map ~f:(convert ~uint ~sint ~trunc) (bits ~f b)
;;

let pad_zero n c = c @: zero n

let%expect_test "Bits.to_signed_int" =
  print_s
    [%message
      ""
        ~should_be_minus_1:(Bits.to_signed_int (Bits.of_bit_string "1") : int)
        ~should_be_minus_2:(Bits.to_signed_int (Bits.of_bit_string "10") : int)
        ~should_be_minus_1:(Bits.to_signed_int (Bits.of_bit_string "11") : int)];
  [%expect
    {|
    ((should_be_minus_1 -1)
     (should_be_minus_2 -2)
     (should_be_minus_1 -1))
    |}]
;;

let%expect_test "to int" =
  let convert_bits =
    convert_bits ~uint:to_unsigned_int ~sint:to_signed_int ~trunc:to_int_trunc
  in
  print_s
    [%message
      ""
        ~bits_1:(convert_bits 1 : int Or_error.t signed_and_unsigned list)
        ~bits_2:(convert_bits 2 : int Or_error.t signed_and_unsigned list)
        ~bits_3:(convert_bits 3 : int Or_error.t signed_and_unsigned list)
        ~bits_4:(convert_bits 4 : int Or_error.t signed_and_unsigned list)];
  [%expect
    {|
    ((bits_1 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok -1))
        (truncated (Ok 1)))))
     (bits_2 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok -2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok -1))
        (truncated (Ok 3)))))
     (bits_3 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok 2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok 3))
        (truncated (Ok 3)))
       ((unsigned  (Ok 4))
        (signed    (Ok -4))
        (truncated (Ok 4)))
       ((unsigned  (Ok 5))
        (signed    (Ok -3))
        (truncated (Ok 5)))
       ((unsigned  (Ok 6))
        (signed    (Ok -2))
        (truncated (Ok 6)))
       ((unsigned  (Ok 7))
        (signed    (Ok -1))
        (truncated (Ok 7)))))
     (bits_4 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok 2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok 3))
        (truncated (Ok 3)))
       ((unsigned  (Ok 4))
        (signed    (Ok 4))
        (truncated (Ok 4)))
       ((unsigned  (Ok 5))
        (signed    (Ok 5))
        (truncated (Ok 5)))
       ((unsigned  (Ok 6))
        (signed    (Ok 6))
        (truncated (Ok 6)))
       ((unsigned  (Ok 7))
        (signed    (Ok 7))
        (truncated (Ok 7)))
       ((unsigned  (Ok 8))
        (signed    (Ok -8))
        (truncated (Ok 8)))
       ((unsigned  (Ok 9))
        (signed    (Ok -7))
        (truncated (Ok 9)))
       ((unsigned  (Ok 10))
        (signed    (Ok -6))
        (truncated (Ok 10)))
       ((unsigned  (Ok 11))
        (signed    (Ok -5))
        (truncated (Ok 11)))
       ((unsigned  (Ok 12))
        (signed    (Ok -4))
        (truncated (Ok 12)))
       ((unsigned  (Ok 13))
        (signed    (Ok -3))
        (truncated (Ok 13)))
       ((unsigned  (Ok 14))
        (signed    (Ok -2))
        (truncated (Ok 14)))
       ((unsigned  (Ok 15))
        (signed    (Ok -1))
        (truncated (Ok 15))))))
    |}]
;;

let%expect_test "to int32" =
  let convert_bits =
    convert_bits ~uint:to_unsigned_int32 ~sint:to_signed_int32 ~trunc:to_int32_trunc
  in
  print_s
    [%message
      ""
        ~bits_1:(convert_bits 1 : int32 Or_error.t signed_and_unsigned list)
        ~bits_2:(convert_bits 2 : int32 Or_error.t signed_and_unsigned list)
        ~bits_3:(convert_bits 3 : int32 Or_error.t signed_and_unsigned list)
        ~bits_4:(convert_bits 4 : int32 Or_error.t signed_and_unsigned list)
        ~at_msb:
          (convert_bits ~f:(pad_zero 30) 2 : int32 Or_error.t signed_and_unsigned list)
        ~across_msb:
          (convert_bits ~f:(pad_zero 61) 2 : int32 Or_error.t signed_and_unsigned list)
        ~above_msb:
          (convert_bits ~f:(pad_zero 32) 2 : int32 Or_error.t signed_and_unsigned list)];
  [%expect
    {|
    ((bits_1 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok -1))
        (truncated (Ok 1)))))
     (bits_2 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok -2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok -1))
        (truncated (Ok 3)))))
     (bits_3 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok 2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok 3))
        (truncated (Ok 3)))
       ((unsigned  (Ok 4))
        (signed    (Ok -4))
        (truncated (Ok 4)))
       ((unsigned  (Ok 5))
        (signed    (Ok -3))
        (truncated (Ok 5)))
       ((unsigned  (Ok 6))
        (signed    (Ok -2))
        (truncated (Ok 6)))
       ((unsigned  (Ok 7))
        (signed    (Ok -1))
        (truncated (Ok 7)))))
     (bits_4 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok 2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok 3))
        (truncated (Ok 3)))
       ((unsigned  (Ok 4))
        (signed    (Ok 4))
        (truncated (Ok 4)))
       ((unsigned  (Ok 5))
        (signed    (Ok 5))
        (truncated (Ok 5)))
       ((unsigned  (Ok 6))
        (signed    (Ok 6))
        (truncated (Ok 6)))
       ((unsigned  (Ok 7))
        (signed    (Ok 7))
        (truncated (Ok 7)))
       ((unsigned  (Ok 8))
        (signed    (Ok -8))
        (truncated (Ok 8)))
       ((unsigned  (Ok 9))
        (signed    (Ok -7))
        (truncated (Ok 9)))
       ((unsigned  (Ok 10))
        (signed    (Ok -6))
        (truncated (Ok 10)))
       ((unsigned  (Ok 11))
        (signed    (Ok -5))
        (truncated (Ok 11)))
       ((unsigned  (Ok 12))
        (signed    (Ok -4))
        (truncated (Ok 12)))
       ((unsigned  (Ok 13))
        (signed    (Ok -3))
        (truncated (Ok 13)))
       ((unsigned  (Ok 14))
        (signed    (Ok -2))
        (truncated (Ok 14)))
       ((unsigned  (Ok 15))
        (signed    (Ok -1))
        (truncated (Ok 15)))))
     (at_msb (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1073741824))
        (signed    (Ok 1073741824))
        (truncated (Ok 1073741824)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -2147483648))
        (truncated (Ok    -2147483648)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -1073741824))
        (truncated (Ok    -1073741824)))))
     (across_msb (
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
        (truncated (Ok    0)))))
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

let%expect_test "to int64" =
  let convert_bits =
    convert_bits ~uint:to_unsigned_int64 ~sint:to_signed_int64 ~trunc:to_int64_trunc
  in
  print_s
    [%message
      ""
        ~bits_1:(convert_bits 1 : int64 Or_error.t signed_and_unsigned list)
        ~bits_2:(convert_bits 2 : int64 Or_error.t signed_and_unsigned list)
        ~bits_3:(convert_bits 3 : int64 Or_error.t signed_and_unsigned list)
        ~bits_4:(convert_bits 4 : int64 Or_error.t signed_and_unsigned list)
        ~at_msb:
          (convert_bits ~f:(pad_zero 62) 2 : int64 Or_error.t signed_and_unsigned list)
        ~across_msb:
          (convert_bits ~f:(pad_zero 63) 2 : int64 Or_error.t signed_and_unsigned list)
        ~above_msb:
          (convert_bits ~f:(pad_zero 64) 2 : int64 Or_error.t signed_and_unsigned list)];
  [%expect
    {|
    ((bits_1 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok -1))
        (truncated (Ok 1)))))
     (bits_2 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok -2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok -1))
        (truncated (Ok 3)))))
     (bits_3 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok 2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok 3))
        (truncated (Ok 3)))
       ((unsigned  (Ok 4))
        (signed    (Ok -4))
        (truncated (Ok 4)))
       ((unsigned  (Ok 5))
        (signed    (Ok -3))
        (truncated (Ok 5)))
       ((unsigned  (Ok 6))
        (signed    (Ok -2))
        (truncated (Ok 6)))
       ((unsigned  (Ok 7))
        (signed    (Ok -1))
        (truncated (Ok 7)))))
     (bits_4 (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 1))
        (signed    (Ok 1))
        (truncated (Ok 1)))
       ((unsigned  (Ok 2))
        (signed    (Ok 2))
        (truncated (Ok 2)))
       ((unsigned  (Ok 3))
        (signed    (Ok 3))
        (truncated (Ok 3)))
       ((unsigned  (Ok 4))
        (signed    (Ok 4))
        (truncated (Ok 4)))
       ((unsigned  (Ok 5))
        (signed    (Ok 5))
        (truncated (Ok 5)))
       ((unsigned  (Ok 6))
        (signed    (Ok 6))
        (truncated (Ok 6)))
       ((unsigned  (Ok 7))
        (signed    (Ok 7))
        (truncated (Ok 7)))
       ((unsigned  (Ok 8))
        (signed    (Ok -8))
        (truncated (Ok 8)))
       ((unsigned  (Ok 9))
        (signed    (Ok -7))
        (truncated (Ok 9)))
       ((unsigned  (Ok 10))
        (signed    (Ok -6))
        (truncated (Ok 10)))
       ((unsigned  (Ok 11))
        (signed    (Ok -5))
        (truncated (Ok 11)))
       ((unsigned  (Ok 12))
        (signed    (Ok -4))
        (truncated (Ok 12)))
       ((unsigned  (Ok 13))
        (signed    (Ok -3))
        (truncated (Ok 13)))
       ((unsigned  (Ok 14))
        (signed    (Ok -2))
        (truncated (Ok 14)))
       ((unsigned  (Ok 15))
        (signed    (Ok -1))
        (truncated (Ok 15)))))
     (at_msb (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Ok 4611686018427387904))
        (signed    (Ok 4611686018427387904))
        (truncated (Ok 4611686018427387904)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -9223372036854775808))
        (truncated (Ok    -9223372036854775808)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -4611686018427387904))
        (truncated (Ok    -4611686018427387904)))))
     (across_msb (
       ((unsigned  (Ok 0))
        (signed    (Ok 0))
        (truncated (Ok 0)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Error "conversion failed"))
        (truncated (Ok    -9223372036854775808)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Error "conversion failed"))
        (truncated (Ok    0)))
       ((unsigned  (Error "conversion failed"))
        (signed    (Ok    -9223372036854775808))
        (truncated (Ok    -9223372036854775808)))))
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

let%expect_test "to_bstr" =
  print_s
    [%sexp
      (List.map
         ~f:(fun c -> of_string c |> to_bstr)
         [ "0"; "1"; "0000"; "1111"; "10011"; "1101000101010101011010" ]
       : string list)];
  [%expect {| (0 1 0000 1111 10011 1101000101010101011010) |}]
;;
