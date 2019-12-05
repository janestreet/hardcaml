open Base
open Hardcaml
open Bits

let%expect_test "a.:[(msb,lsb)]" =
  let x = of_int ~width:32 0xdeadbeef in
  Stdio.printf !"x = %{Bits}\n" x;
  let indices = [ 31, 0; 5, 0; 14, 7; 23, 19 ] in
  List.iter indices ~f:(fun (hi, lo) ->
    Stdio.printf !"  x.:[(%d,%d)] = %{Bits}\n" hi lo (x.:[(hi, lo)]));
  [%expect
    {|
    x = 11011110101011011011111011101111
      x.:[(31,0)] = 11011110101011011011111011101111
      x.:[(5,0)] = 101111
      x.:[(14,7)] = 01111101
      x.:[(23,19)] = 10101 |}]
;;

let%expect_test "a.:(bit)" =
  let x = of_int ~width:32 0xdeadbeef in
  Stdio.printf !"x = %{Bits}\n" x;
  let indices = [ 31; 29; 23; 19; 7 ] in
  List.iter indices ~f:(fun i -> Stdio.printf !"  x.:(%d) = %{Bits}\n" i (x.:(i)));
  [%expect
    {|
    x = 11011110101011011011111011101111
      x.:(31) = 1
      x.:(29) = 0
      x.:(23) = 1
      x.:(19) = 1
      x.:(7) = 1 |}]
;;

let print_option = function
  | None -> "None"
  | Some x -> "Some " ^ Int.to_string x
;;

let%expect_test "a.:+[(lsb, width option)]" =
  let x = of_int ~width:32 0xdeadbeef in
  Stdio.printf !"x = %{Bits}\n" x;
  let indices = [ 0, Some 5; 4, Some 7; 20, None ] in
  List.iter indices ~f:(fun (lsb, width) ->
    Stdio.printf
      !"  x.:+[(%d, %{print_option})] = %{Bits}\n"
      lsb
      width
      (x.:+[(lsb, width)]));
  [%expect
    {|
    x = 11011110101011011011111011101111
      x.:+[(0, Some 5)] = 01111
      x.:+[(4, Some 7)] = 1101110
      x.:+[(20, None)] = 110111101010 |}]
;;

let%expect_test "a.:-[(msb option, width)]" =
  let x = of_int ~width:32 0xdeadbeef in
  Stdio.printf !"x = %{Bits}\n" x;
  let indices = [ Some 31, 7; Some 10, 10; None, 20 ] in
  List.iter indices ~f:(fun (msb, width) ->
    Stdio.printf
      !"  x.:+[(%{print_option}, %d)] = %{Bits}\n"
      msb
      width
      (x.:-[(msb, width)]));
  [%expect
    {|
    x = 11011110101011011011111011101111
      x.:+[(Some 31, 7)] = 1101111
      x.:+[(Some 10, 10)] = 1101110111
      x.:+[(None, 20)] = 11011110101011011011 |}]
;;
