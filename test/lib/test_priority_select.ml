(* Test [priority_select] (and related) functions. *)

open! Import

let cases index =
  index
  |> Bits.bits_lsb
  |> List.mapi ~f:(fun i valid ->
    { With_valid.valid; value = Bits.of_int ~width:8 (i + 1) })
;;

let sexp_of_int_with_valid (i : int With_valid.t) =
  match i.valid with
  | 0 -> [%message "<invalid>"]
  | 1 -> [%sexp (i.value : int)]
  | _ -> assert false
;;

let test ~branching_factor ~case_count =
  print_s [%message (branching_factor : int) (case_count : int)];
  for index = 0 to (1 lsl case_count) - 1 do
    let index = Bits.of_int ~width:case_count index in
    print_s
      [%message
        "priority_select"
          ~_:(index : Bits.t)
          "="
          ~_:
            (cases index
             |> Bits.priority_select ~branching_factor
             |> With_valid.map ~f:Bits.to_int
             : int_with_valid)]
  done
;;

let%expect_test "empty data" =
  show_raise (fun () -> Bits.priority_select []);
  [%expect {|
    (raised "[tree_or_reduce_binary_operator] got empty list") |}]
;;

let%expect_test "branching factor 1" =
  for case_count = 1 to 4 do
    test ~branching_factor:1 ~case_count
  done;
  [%expect
    {|
    ((branching_factor 1)
     (case_count       1))
    (priority_select 0 = <invalid>)
    (priority_select 1 = 1)
    ((branching_factor 1)
     (case_count       2))
    (priority_select 00 = <invalid>)
    (priority_select 01 = 1)
    (priority_select 10 = 2)
    (priority_select 11 = 1)
    ((branching_factor 1)
     (case_count       3))
    (priority_select 000 = <invalid>)
    (priority_select 001 = 1)
    (priority_select 010 = 2)
    (priority_select 011 = 1)
    (priority_select 100 = 3)
    (priority_select 101 = 1)
    (priority_select 110 = 2)
    (priority_select 111 = 1)
    ((branching_factor 1)
     (case_count       4))
    (priority_select 0000 = <invalid>)
    (priority_select 0001 = 1)
    (priority_select 0010 = 2)
    (priority_select 0011 = 1)
    (priority_select 0100 = 3)
    (priority_select 0101 = 1)
    (priority_select 0110 = 2)
    (priority_select 0111 = 1)
    (priority_select 1000 = 4)
    (priority_select 1001 = 1)
    (priority_select 1010 = 2)
    (priority_select 1011 = 1)
    (priority_select 1100 = 3)
    (priority_select 1101 = 1)
    (priority_select 1110 = 2)
    (priority_select 1111 = 1) |}]
;;

let%expect_test "branching factor 2" =
  test ~branching_factor:2 ~case_count:4;
  [%expect
    {|
    ((branching_factor 2)
     (case_count       4))
    (priority_select 0000 = <invalid>)
    (priority_select 0001 = 1)
    (priority_select 0010 = 2)
    (priority_select 0011 = 1)
    (priority_select 0100 = 3)
    (priority_select 0101 = 1)
    (priority_select 0110 = 2)
    (priority_select 0111 = 1)
    (priority_select 1000 = 4)
    (priority_select 1001 = 1)
    (priority_select 1010 = 2)
    (priority_select 1011 = 1)
    (priority_select 1100 = 3)
    (priority_select 1101 = 1)
    (priority_select 1110 = 2)
    (priority_select 1111 = 1) |}]
;;

let%expect_test "branching factor 3" =
  test ~branching_factor:3 ~case_count:4;
  [%expect
    {|
    ((branching_factor 3)
     (case_count       4))
    (priority_select 0000 = <invalid>)
    (priority_select 0001 = 1)
    (priority_select 0010 = 2)
    (priority_select 0011 = 1)
    (priority_select 0100 = 3)
    (priority_select 0101 = 1)
    (priority_select 0110 = 2)
    (priority_select 0111 = 1)
    (priority_select 1000 = 4)
    (priority_select 1001 = 1)
    (priority_select 1010 = 2)
    (priority_select 1011 = 1)
    (priority_select 1100 = 3)
    (priority_select 1101 = 1)
    (priority_select 1110 = 2)
    (priority_select 1111 = 1) |}]
;;

let%expect_test "branching factor 4" =
  test ~branching_factor:4 ~case_count:5;
  [%expect
    {|
    ((branching_factor 4)
     (case_count       5))
    (priority_select 00000 = <invalid>)
    (priority_select 00001 = 1)
    (priority_select 00010 = 2)
    (priority_select 00011 = 1)
    (priority_select 00100 = 3)
    (priority_select 00101 = 1)
    (priority_select 00110 = 2)
    (priority_select 00111 = 1)
    (priority_select 01000 = 4)
    (priority_select 01001 = 1)
    (priority_select 01010 = 2)
    (priority_select 01011 = 1)
    (priority_select 01100 = 3)
    (priority_select 01101 = 1)
    (priority_select 01110 = 2)
    (priority_select 01111 = 1)
    (priority_select 10000 = 5)
    (priority_select 10001 = 1)
    (priority_select 10010 = 2)
    (priority_select 10011 = 1)
    (priority_select 10100 = 3)
    (priority_select 10101 = 1)
    (priority_select 10110 = 2)
    (priority_select 10111 = 1)
    (priority_select 11000 = 4)
    (priority_select 11001 = 1)
    (priority_select 11010 = 2)
    (priority_select 11011 = 1)
    (priority_select 11100 = 3)
    (priority_select 11101 = 1)
    (priority_select 11110 = 2)
    (priority_select 11111 = 1) |}]
;;

let test_with_default ~branching_factor ~case_count =
  print_s [%message (branching_factor : int) (case_count : int)];
  for index = 0 to (1 lsl case_count) - 1 do
    let index = Bits.of_int ~width:case_count index in
    print_s
      [%message
        "priority_select_with_default"
          ~_:(index : Bits.t)
          "="
          ~_:
            (cases index
             |> Bits.priority_select_with_default
                  ~branching_factor
                  ~default:(Bits.ones 8)
             |> Bits.to_int
             : int)]
  done
;;

let%expect_test "with default" =
  test_with_default ~branching_factor:1 ~case_count:1;
  [%expect
    {|
    ((branching_factor 1)
     (case_count       1))
    (priority_select_with_default 0 = 255)
    (priority_select_with_default 1 = 1) |}];
  test_with_default ~branching_factor:3 ~case_count:5;
  [%expect
    {|
    ((branching_factor 3)
     (case_count       5))
    (priority_select_with_default 00000 = 255)
    (priority_select_with_default 00001 = 1)
    (priority_select_with_default 00010 = 2)
    (priority_select_with_default 00011 = 1)
    (priority_select_with_default 00100 = 3)
    (priority_select_with_default 00101 = 1)
    (priority_select_with_default 00110 = 2)
    (priority_select_with_default 00111 = 1)
    (priority_select_with_default 01000 = 4)
    (priority_select_with_default 01001 = 1)
    (priority_select_with_default 01010 = 2)
    (priority_select_with_default 01011 = 1)
    (priority_select_with_default 01100 = 3)
    (priority_select_with_default 01101 = 1)
    (priority_select_with_default 01110 = 2)
    (priority_select_with_default 01111 = 1)
    (priority_select_with_default 10000 = 5)
    (priority_select_with_default 10001 = 1)
    (priority_select_with_default 10010 = 2)
    (priority_select_with_default 10011 = 1)
    (priority_select_with_default 10100 = 3)
    (priority_select_with_default 10101 = 1)
    (priority_select_with_default 10110 = 2)
    (priority_select_with_default 10111 = 1)
    (priority_select_with_default 11000 = 4)
    (priority_select_with_default 11001 = 1)
    (priority_select_with_default 11010 = 2)
    (priority_select_with_default 11011 = 1)
    (priority_select_with_default 11100 = 3)
    (priority_select_with_default 11101 = 1)
    (priority_select_with_default 11110 = 2)
    (priority_select_with_default 11111 = 1) |}]
;;

let test_onehot ~branching_factor ~case_count =
  print_s [%message (branching_factor : int) (case_count : int)];
  for index = 0 to (1 lsl case_count) - 1 do
    let index = Bits.of_int ~width:case_count index in
    print_s
      [%message
        "onehot_select"
          ~_:(index : Bits.t)
          "="
          ~_:(cases index |> Bits.onehot_select ~branching_factor |> Bits.to_int : int)]
  done
;;

let%expect_test "onehot" =
  test_onehot ~branching_factor:1 ~case_count:1;
  [%expect
    {|
    ((branching_factor 1)
     (case_count       1))
    (onehot_select 0 = 0)
    (onehot_select 1 = 1) |}];
  test_onehot ~branching_factor:2 ~case_count:4;
  [%expect
    {|
    ((branching_factor 2)
     (case_count       4))
    (onehot_select 0000 = 0)
    (onehot_select 0001 = 1)
    (onehot_select 0010 = 2)
    (onehot_select 0011 = 3)
    (onehot_select 0100 = 3)
    (onehot_select 0101 = 3)
    (onehot_select 0110 = 3)
    (onehot_select 0111 = 3)
    (onehot_select 1000 = 4)
    (onehot_select 1001 = 5)
    (onehot_select 1010 = 6)
    (onehot_select 1011 = 7)
    (onehot_select 1100 = 7)
    (onehot_select 1101 = 7)
    (onehot_select 1110 = 7)
    (onehot_select 1111 = 7) |}]
;;
