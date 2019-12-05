(* Test [Bits] functions:
   - [leading_zeros]
   - [leading_ones]
   - [trailing_zeros]
   - [trailing_ones] *)

open! Import

let%expect_test "bits = 1, 2, 3, 4" =
  let sexp_of_result t = [%sexp (t : (Bits.t * int) list)] in
  for bits = 1 to 4 do
    let test f =
      List.init (1 lsl bits) ~f:(fun i ->
        let c = Bits.of_int ~width:bits i in
        c, f c |> Bits.to_int)
    in
    print_s
      [%message
        (bits : int)
          ~leading_zeros:(test Bits.leading_zeros : result)
          ~leading_ones:(test Bits.leading_ones : result)
          ~trailing_zeros:(test Bits.trailing_zeros : result)
          ~trailing_ones:(test Bits.trailing_ones : result)]
  done;
  [%expect
    {|
    ((bits 1)
     (leading_zeros  ((0 1) (1 0)))
     (leading_ones   ((0 0) (1 1)))
     (trailing_zeros ((0 1) (1 0)))
     (trailing_ones  ((0 0) (1 1))))
    ((bits 2)
     (leading_zeros (
       (00 2)
       (01 1)
       (10 0)
       (11 0)))
     (leading_ones (
       (00 0)
       (01 0)
       (10 1)
       (11 2)))
     (trailing_zeros (
       (00 2)
       (01 0)
       (10 1)
       (11 0)))
     (trailing_ones (
       (00 0)
       (01 1)
       (10 0)
       (11 2))))
    ((bits 3)
     (leading_zeros (
       (000 3)
       (001 2)
       (010 1)
       (011 1)
       (100 0)
       (101 0)
       (110 0)
       (111 0)))
     (leading_ones (
       (000 0)
       (001 0)
       (010 0)
       (011 0)
       (100 1)
       (101 1)
       (110 2)
       (111 3)))
     (trailing_zeros (
       (000 3)
       (001 0)
       (010 1)
       (011 0)
       (100 2)
       (101 0)
       (110 1)
       (111 0)))
     (trailing_ones (
       (000 0)
       (001 1)
       (010 0)
       (011 2)
       (100 0)
       (101 1)
       (110 0)
       (111 3))))
    ((bits 4)
     (leading_zeros (
       (0000 4)
       (0001 3)
       (0010 2)
       (0011 2)
       (0100 1)
       (0101 1)
       (0110 1)
       (0111 1)
       (1000 0)
       (1001 0)
       (1010 0)
       (1011 0)
       (1100 0)
       (1101 0)
       (1110 0)
       (1111 0)))
     (leading_ones (
       (0000 0)
       (0001 0)
       (0010 0)
       (0011 0)
       (0100 0)
       (0101 0)
       (0110 0)
       (0111 0)
       (1000 1)
       (1001 1)
       (1010 1)
       (1011 1)
       (1100 2)
       (1101 2)
       (1110 3)
       (1111 4)))
     (trailing_zeros (
       (0000 4)
       (0001 0)
       (0010 1)
       (0011 0)
       (0100 2)
       (0101 0)
       (0110 1)
       (0111 0)
       (1000 3)
       (1001 0)
       (1010 1)
       (1011 0)
       (1100 2)
       (1101 0)
       (1110 1)
       (1111 0)))
     (trailing_ones (
       (0000 0)
       (0001 1)
       (0010 0)
       (0011 2)
       (0100 0)
       (0101 1)
       (0110 0)
       (0111 3)
       (1000 0)
       (1001 1)
       (1010 0)
       (1011 2)
       (1100 0)
       (1101 1)
       (1110 0)
       (1111 4)))) |}]
;;

let%expect_test "random" =
  for _ = 0 to 99 do
    let bits = Random.int 8 + 1 in
    let value = Bits.random ~width:bits in
    let hw = Bits.leading_zeros value |> Bits.to_int in
    let value = Bits.to_int value in
    let sw = if value = 0 then bits else bits - (Int.floor_log2 value + 1) in
    require
      [%here]
      (hw = sw)
      ~if_false_then_print_s:
        (lazy [%message "" ~_:(bits, value, hw, sw : int * int * int * int)])
  done;
  [%expect {| |}]
;;
