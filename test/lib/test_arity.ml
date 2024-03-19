open! Import
open! Bits

let%expect_test "[compute_arity]" =
  let test ~num_steps value =
    let arities =
      List.init num_steps ~f:(fun steps -> Bits.compute_arity ~steps:(steps + 1) value)
    in
    print_s [%message (arities : int list)]
  in
  test ~num_steps:5 0;
  [%expect {| (arities (0 2 2 2 2)) |}];
  test ~num_steps:5 1;
  [%expect {| (arities (1 1 1 1 1)) |}];
  test ~num_steps:5 2;
  [%expect {| (arities (2 2 2 2 2)) |}];
  test ~num_steps:5 30;
  [%expect {| (arities (30 6 4 3 2)) |}]
;;

let%expect_test "[compute_tree_branches]" =
  let test ~steps num_values =
    let branches = Bits.compute_tree_branches ~steps num_values in
    let branches, last_value =
      List.fold ~init:([], num_values) branches ~f:(fun (lst, num_values) branch ->
        let num_values' = (num_values + branch - 1) / branch in
        (branch, num_values) :: lst, num_values')
    in
    let num_values = last_value :: List.map branches ~f:snd |> List.rev in
    let branches = List.map branches ~f:fst |> List.rev in
    print_s [%message (branches : int list) (num_values : int list)]
  in
  (* zero steps - non-sensicle anyway *)
  test ~steps:0 0;
  [%expect {| ((branches ()) (num_values (0))) |}];
  test ~steps:0 1;
  [%expect {| ((branches ()) (num_values (1))) |}];
  test ~steps:0 4;
  [%expect {| ((branches ()) (num_values (4))) |}];
  (* one step *)
  test ~steps:1 0;
  [%expect {| ((branches ()) (num_values (0))) |}];
  test ~steps:1 1;
  [%expect {| ((branches (1)) (num_values (1 1))) |}];
  test ~steps:1 4;
  [%expect {| ((branches (4)) (num_values (4 1))) |}];
  (* two steps *)
  for num_values = 0 to 8 do
    test ~steps:2 num_values
  done;
  [%expect
    {|
    ((branches ()) (num_values (0)))
    ((branches (1 1)) (num_values (1 1 1)))
    ((branches (2 1)) (num_values (2 1 1)))
    ((branches (2 2)) (num_values (3 2 1)))
    ((branches (2 2)) (num_values (4 2 1)))
    ((branches (3 2)) (num_values (5 2 1)))
    ((branches (3 2)) (num_values (6 2 1)))
    ((branches (3 3)) (num_values (7 3 1)))
    ((branches (3 3)) (num_values (8 3 1)))
    |}];
  (* three steps *)
  for num_values = 0 to 8 do
    test ~steps:3 num_values
  done;
  [%expect
    {|
    ((branches ()) (num_values (0)))
    ((branches (1 1 1)) (num_values (1 1 1 1)))
    ((branches (2 1 1)) (num_values (2 1 1 1)))
    ((branches (2 2 1)) (num_values (3 2 1 1)))
    ((branches (2 2 1)) (num_values (4 2 1 1)))
    ((branches (2 2 2)) (num_values (5 3 2 1)))
    ((branches (2 2 2)) (num_values (6 3 2 1)))
    ((branches (2 2 2)) (num_values (7 4 2 1)))
    ((branches (2 2 2)) (num_values (8 4 2 1)))
    |}];
  (* more steps that required *)
  test ~steps:10 1;
  test ~steps:10 2;
  test ~steps:10 5;
  test ~steps:10 9;
  test ~steps:10 10;
  [%expect
    {|
    ((branches (1 1 1 1 1 1 1 1 1 1)) (num_values (1 1 1 1 1 1 1 1 1 1 1)))
    ((branches (2 1 1 1 1 1 1 1 1 1)) (num_values (2 1 1 1 1 1 1 1 1 1 1)))
    ((branches (2 2 2 1 1 1 1 1 1 1)) (num_values (5 3 2 1 1 1 1 1 1 1 1)))
    ((branches (2 2 2 2 1 1 1 1 1 1)) (num_values (9 5 3 2 1 1 1 1 1 1 1)))
    ((branches (2 2 2 2 1 1 1 1 1 1)) (num_values (10 5 3 2 1 1 1 1 1 1 1)))
    |}];
  (* larger examples *)
  test ~steps:5 1356;
  [%expect {| ((branches (5 5 4 4 4)) (num_values (1356 272 55 14 4 1))) |}];
  test ~steps:3 18;
  [%expect {| ((branches (3 3 2)) (num_values (18 6 2 1))) |}];
  test ~steps:10 100_000_000;
  [%expect
    {|
    ((branches (7 7 7 7 6 6 6 6 6 6))
     (num_values (100000000 14285715 2040817 291546 41650 6942 1157 193 33 6 1)))
    |}];
  test ~steps:4 17;
  [%expect {| ((branches (3 2 2 2)) (num_values (17 6 3 2 1))) |}]
;;
