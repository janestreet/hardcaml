open! Base
open Expect_test_helpers_base
open Hardcaml.Signal

let%expect_test "mux select errors" =
  require_does_not_raise (fun () ->
    ignore (mux (of_unsigned_int ~width:61 0) [ vdd ] : t));
  [%expect {| |}];
  require_does_raise (fun () -> mux (of_unsigned_int ~width:62 0) [ vdd ]);
  [%expect
    {|
    ("[mux] select width must fit within an (unsigned) integer value"
     (width_of_sel        62)
     (max_mux_select_bits 61))
    |}];
  require_does_raise (fun () -> mux (of_unsigned_int ~width:10_000 0) [ vdd ]);
  [%expect
    {|
    ("[mux] select width must fit within an (unsigned) integer value"
     (width_of_sel        10000)
     (max_mux_select_bits 61))
    |}]
;;
