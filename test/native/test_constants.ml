open Base
open Expect_test_helpers_base
open Hardcaml_test.Test_constants

let%expect_test "minimum and maximum (64 bit ocaml, native int)" =
  print_s [%message "" ~of_int:(min_max of_int (module Int) : of_int min_max)];
  [%expect
    {|
    (of_int (
      (min ((63 -4611686018427387904) 63'h4000000000000000))
      (max ((63 4611686018427387903)  63'h3fffffffffffffff))))
    |}]
;;
