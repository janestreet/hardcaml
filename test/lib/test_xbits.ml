open! Import
module Bits = Bits_list.X_comb

let x = Bits.of_string "000111xxx"
let y = Bits.of_string "01x01x01x"

let test_op f =
  print_s [%message (x : Bits.t)];
  print_s [%message (y : Bits.t)];
  print_s [%message "" ~r:(f x y : Bits.t)]
;;

let%expect_test "&:" =
  test_op Bits.( &: );
  [%expect {|
    (x 000111xxx)
    (y 01x01x01x)
    (r 00x01xxxx)
    |}]
;;

let%expect_test "|:" =
  test_op Bits.( |: );
  [%expect {|
    (x 000111xxx)
    (y 01x01x01x)
    (r 01x11xxxx)
    |}]
;;

let%expect_test "^:" =
  test_op Bits.( ^: );
  [%expect {|
    (x 000111xxx)
    (y 01x01x01x)
    (r 01x10xxxx)
    |}]
;;

let%expect_test "mux with x's in index" =
  let index = Bits.of_string "1x" in
  let result = Bits.mux index (List.init 4 ~f:(Bits.of_int ~width:2)) in
  print_s [%message (result : Bits.t)];
  [%expect {| (result xx) |}]
;;
