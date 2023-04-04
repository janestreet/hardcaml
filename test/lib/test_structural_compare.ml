open! Import
open Signal

let circuit ~circuit_name ~internal_name ~input_count ~output_count =
  let i = List.init input_count ~f:(fun j -> input ("i" ^ Int.to_string j) 1) in
  let o = reduce ~f:( |: ) i -- internal_name in
  let o = List.init output_count ~f:(fun j -> output ("o" ^ Int.to_string j) o) in
  Circuit.create_exn ~name:circuit_name o
;;

let%expect_test "simple comparisons" =
  let compare =
    Circuit.structural_compare
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:2 ~output_count:1)
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:2 ~output_count:1)
  in
  print_s [%message "circuits same" (compare : bool)];
  [%expect {| ("circuits same" (compare true)) |}];
  let compare =
    Circuit.structural_compare
      (circuit ~circuit_name:"test1" ~internal_name:"foo" ~input_count:2 ~output_count:1)
      (circuit ~circuit_name:"test2" ~internal_name:"foo" ~input_count:2 ~output_count:1)
  in
  print_s [%message "circuits names differ" (compare : bool)];
  [%expect {| ("circuits names differ" (compare true)) |}];
  let compare =
    Circuit.structural_compare
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:2 ~output_count:1)
      (circuit ~circuit_name:"test" ~internal_name:"goo" ~input_count:2 ~output_count:1)
  in
  print_s [%message "circuits internal names differ" (compare : bool)];
  [%expect {| ("circuits internal names differ" (compare false)) |}];
  let compare =
    Circuit.structural_compare
      ~check_names:false
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:2 ~output_count:1)
      (circuit ~circuit_name:"test" ~internal_name:"goo" ~input_count:2 ~output_count:1)
  in
  print_s [%message "circuits internal names differ (but dont care)" (compare : bool)];
  [%expect {| ("circuits internal names differ (but dont care)" (compare true)) |}]
;;

let%expect_test "different number of inputs" =
  let compare =
    Circuit.structural_compare
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:2 ~output_count:1)
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:3 ~output_count:1)
  in
  print_s [%message "circuits same" (compare : bool)];
  [%expect {|
    ("circuits same" (compare false)) |}]
;;

let%expect_test "different number of outputs" =
  let compare =
    Circuit.structural_compare
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:2 ~output_count:1)
      (circuit ~circuit_name:"test" ~internal_name:"foo" ~input_count:2 ~output_count:2)
  in
  print_s [%message "circuits same" (compare : bool)];
  [%expect {|
    ("circuits same" (compare false)) |}]
;;
