open Core
open Hardcaml
open Bits
module E = Expect_test_helpers_base

type test_cases =
  { int : int
  ; int32 : int32
  ; int64 : int64
  ; bigint : Bigint.t
  }

let test_all_unsigned_int_functions { int; int32; int64; bigint } ~width f =
  print_endline "of_unsigned_int";
  print_endline "----";
  f (fun () -> of_unsigned_int ~width int);
  print_endline "";
  print_endline "of_unsigned_int32";
  print_endline "----";
  f (fun () -> of_unsigned_int32 ~width int32);
  print_endline "";
  print_endline "of_unsigned_int64";
  print_endline "----";
  f (fun () -> of_unsigned_int64 ~width int64);
  print_endline "";
  print_endline "of_unsigned_bigint";
  print_endline "----";
  f (fun () -> of_unsigned_bigint ~width bigint)
;;

let test_all_signed_int_functions { int; int32; int64; bigint } ~width f =
  print_endline "of_signed_int";
  print_endline "----";
  f (fun () -> of_signed_int ~width int);
  print_endline "";
  print_endline "of_signed_int32";
  print_endline "----";
  f (fun () -> of_signed_int32 ~width int32);
  print_endline "";
  print_endline "of_signed_int64";
  print_endline "----";
  f (fun () -> of_signed_int64 ~width int64);
  print_endline "";
  print_endline "of_signed_bigint";
  print_endline "----";
  f (fun () -> of_signed_bigint ~width bigint)
;;

let%expect_test "of_unsigned raises with unrepresentable values" =
  test_all_unsigned_int_functions
    ~width:8
    { int = -1; int32 = -1l; int64 = -1L; bigint = Bigint.of_int (-1) }
    E.require_does_raise;
  [%expect
    {|
    of_unsigned_int
    ----
    ("[of_unsigned_int] input value is less than 0" (x -1))

    of_unsigned_int32
    ----
    ("[of_unsigned_int] input value is less than 0" (x -1))

    of_unsigned_int64
    ----
    ("[of_unsigned_int] input value is less than 0" (x -1))

    of_unsigned_bigint
    ----
    ("[of_unsigned_int] input value is less than 0" (x -1))
    |}];
  test_all_unsigned_int_functions
    ~width:8
    { int = 256; int32 = 256l; int64 = 256L; bigint = Bigint.of_int 256 }
    E.require_does_raise;
  [%expect
    {|
    of_unsigned_int
    ----
    ("[of_unsigned_int] input value is too large for given width"
     (width     8)
     (max_value 255)
     (x         256))

    of_unsigned_int32
    ----
    ("[of_unsigned_int] input value is too large for given width"
     (width     8)
     (max_value 255)
     (x         256))

    of_unsigned_int64
    ----
    ("[of_unsigned_int] input value is too large for given width"
     (width     8)
     (max_value 255)
     (x         256))

    of_unsigned_bigint
    ----
    ("[of_unsigned_int] input value is too large for given width"
     (width     8)
     (max_value 255)
     (x         256))
    |}]
;;

let%expect_test "of_signed raises with unrepresentable values" =
  test_all_signed_int_functions
    ~width:8
    { int = -129; int32 = -129l; int64 = -129L; bigint = Bigint.of_int (-129) }
    E.require_does_raise;
  [%expect
    {|
    of_signed_int
    ----
    ("[of_signed_int] input value is too small for given width"
     (width     8)
     (min_value -128)
     (x         -129))

    of_signed_int32
    ----
    ("[of_signed_int] input value is too small for given width"
     (width     8)
     (min_value -128)
     (x         -129))

    of_signed_int64
    ----
    ("[of_signed_int] input value is too small for given width"
     (width     8)
     (min_value -128)
     (x         -129))

    of_signed_bigint
    ----
    ("[of_signed_int] input value is too small for given width"
     (width     8)
     (min_value -128)
     (x         -129))
    |}];
  test_all_signed_int_functions
    ~width:8
    { int = 128; int32 = 128l; int64 = 128L; bigint = Bigint.of_int 128 }
    E.require_does_raise;
  [%expect
    {|
    of_signed_int
    ----
    ("[of_signed_int] input value is too large for given width"
     (width     8)
     (max_value 127)
     (x         128))

    of_signed_int32
    ----
    ("[of_signed_int] input value is too large for given width"
     (width     8)
     (max_value 127)
     (x         128))

    of_signed_int64
    ----
    ("[of_signed_int] input value is too large for given width"
     (width     8)
     (max_value 127)
     (x         128))

    of_signed_bigint
    ----
    ("[of_signed_int] input value is too large for given width"
     (width     8)
     (max_value 127)
     (x         128))
    |}]
;;

let%expect_test "Work appropriately even when width is larger than the int datatype's \
                 width"
  =
  test_all_signed_int_functions
    ~width:70
    { int = 42; int32 = 42l; int64 = 42L; bigint = Bigint.of_int 42 }
    (fun f -> print_endline (Bits.to_string (f ())));
  [%expect
    {|
    of_signed_int
    ----
    0000000000000000000000000000000000000000000000000000000000000000101010

    of_signed_int32
    ----
    0000000000000000000000000000000000000000000000000000000000000000101010

    of_signed_int64
    ----
    0000000000000000000000000000000000000000000000000000000000000000101010

    of_signed_bigint
    ----
    0000000000000000000000000000000000000000000000000000000000000000101010
    |}]
;;
