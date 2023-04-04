(* Test [is_pow2], [floor_log2] and [ceil_log2]. *)

open! Import

let%expect_test "[is_pow2]" =
  for bits = 1 to 4 do
    print_s [%message (bits : int)];
    for i = 0 to (1 lsl bits) - 1 do
      print_s
        [%message
          "is_pow2"
            ~_:(i : int)
            "="
            ~_:(Bits.is_pow2 (Bits.of_int ~width:bits i) : Bits.t)]
    done
  done;
  [%expect
    {|
    (bits 1)
    (is_pow2 0 = 0)
    (is_pow2 1 = 1)
    (bits 2)
    (is_pow2 0 = 0)
    (is_pow2 1 = 1)
    (is_pow2 2 = 1)
    (is_pow2 3 = 0)
    (bits 3)
    (is_pow2 0 = 0)
    (is_pow2 1 = 1)
    (is_pow2 2 = 1)
    (is_pow2 3 = 0)
    (is_pow2 4 = 1)
    (is_pow2 5 = 0)
    (is_pow2 6 = 0)
    (is_pow2 7 = 0)
    (bits 4)
    (is_pow2 0 = 0)
    (is_pow2 1 = 1)
    (is_pow2 2 = 1)
    (is_pow2 3 = 0)
    (is_pow2 4 = 1)
    (is_pow2 5 = 0)
    (is_pow2 6 = 0)
    (is_pow2 7 = 0)
    (is_pow2 8 = 1)
    (is_pow2 9 = 0)
    (is_pow2 10 = 0)
    (is_pow2 11 = 0)
    (is_pow2 12 = 0)
    (is_pow2 13 = 0)
    (is_pow2 14 = 0)
    (is_pow2 15 = 0) |}]
;;

let sexp_of_bits_with_valid (t : Bits.t With_valid.t) =
  if Bits.to_int t.valid = 1
  then [%sexp (t.value |> Bits.to_int : int)]
  else [%message "<invalid>"]
;;

let test name (bits_f : Bits.t -> Bits.t With_valid.t) int_f =
  for bits = 1 to 4 do
    print_s [%message (bits : int)];
    for i = 0 to (1 lsl bits) - 1 do
      let result = bits_f (Bits.of_int ~width:bits i) in
      (match int_f i with
       | exception _ -> require [%here] (Bits.is_gnd result.valid)
       | x -> require_equal [%here] (module Int) x (result.value |> Bits.to_int));
      print_s [%message name ~_:(i : int) "=" ~_:(result : bits_with_valid)]
    done
  done
;;

let%expect_test "[floor_log2]" =
  test "floor_log2" Bits.floor_log2 Int.floor_log2;
  [%expect
    {|
    (bits 1)
    (floor_log2 0 = <invalid>)
    (floor_log2 1 = 0)
    (bits 2)
    (floor_log2 0 = <invalid>)
    (floor_log2 1 = 0)
    (floor_log2 2 = 1)
    (floor_log2 3 = 1)
    (bits 3)
    (floor_log2 0 = <invalid>)
    (floor_log2 1 = 0)
    (floor_log2 2 = 1)
    (floor_log2 3 = 1)
    (floor_log2 4 = 2)
    (floor_log2 5 = 2)
    (floor_log2 6 = 2)
    (floor_log2 7 = 2)
    (bits 4)
    (floor_log2 0 = <invalid>)
    (floor_log2 1 = 0)
    (floor_log2 2 = 1)
    (floor_log2 3 = 1)
    (floor_log2 4 = 2)
    (floor_log2 5 = 2)
    (floor_log2 6 = 2)
    (floor_log2 7 = 2)
    (floor_log2 8 = 3)
    (floor_log2 9 = 3)
    (floor_log2 10 = 3)
    (floor_log2 11 = 3)
    (floor_log2 12 = 3)
    (floor_log2 13 = 3)
    (floor_log2 14 = 3)
    (floor_log2 15 = 3) |}]
;;

let%expect_test "ceil_log2" =
  test "ceil_log2" Bits.ceil_log2 Int.ceil_log2;
  [%expect
    {|
    (bits 1)
    (ceil_log2 0 = <invalid>)
    (ceil_log2 1 = 0)
    (bits 2)
    (ceil_log2 0 = <invalid>)
    (ceil_log2 1 = 0)
    (ceil_log2 2 = 1)
    (ceil_log2 3 = 2)
    (bits 3)
    (ceil_log2 0 = <invalid>)
    (ceil_log2 1 = 0)
    (ceil_log2 2 = 1)
    (ceil_log2 3 = 2)
    (ceil_log2 4 = 2)
    (ceil_log2 5 = 3)
    (ceil_log2 6 = 3)
    (ceil_log2 7 = 3)
    (bits 4)
    (ceil_log2 0 = <invalid>)
    (ceil_log2 1 = 0)
    (ceil_log2 2 = 1)
    (ceil_log2 3 = 2)
    (ceil_log2 4 = 2)
    (ceil_log2 5 = 3)
    (ceil_log2 6 = 3)
    (ceil_log2 7 = 3)
    (ceil_log2 8 = 3)
    (ceil_log2 9 = 4)
    (ceil_log2 10 = 4)
    (ceil_log2 11 = 4)
    (ceil_log2 12 = 4)
    (ceil_log2 13 = 4)
    (ceil_log2 14 = 4)
    (ceil_log2 15 = 4) |}]
;;
