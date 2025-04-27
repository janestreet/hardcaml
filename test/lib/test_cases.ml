open Core
open Hardcaml
open Bits
open Expect_test_helpers_base
module Cases = Comb.Expert.Gen_cases_from_mux (Bits)

let cases ~default select cases' =
  let q0 = cases ~default select cases' in
  let q1 = Cases.cases ~default select cases' in
  if not (equal q0 q1) then raise_s [%message "Implementation error"];
  q0
;;

let%expect_test "cases" =
  let default = of_string "111" in
  let cases' =
    [ "00", "001"; "10", "110" ] |> List.map ~f:(fun (m, v) -> of_string m, of_string v)
  in
  let q = List.init 4 ~f:(fun i -> cases ~default (of_int_trunc ~width:2 i) cases') in
  print_s [%message (q : t list)];
  [%expect {| (q (001 111 110 111)) |}]
;;

let%expect_test "first match output" =
  let default = of_string "111" in
  let cases' =
    [ "00", "001"; "10", "110"; "10", "000" ]
    |> List.map ~f:(fun (m, v) -> of_string m, of_string v)
  in
  let q = List.init 4 ~f:(fun i -> cases ~default (of_int_trunc ~width:2 i) cases') in
  print_s [%message (q : t list)];
  [%expect {| (q (001 111 110 111)) |}]
;;

let%expect_test "exns" =
  require_does_raise (fun () ->
    cases ~default:(of_string "00") (of_string "000") [ of_string "00", of_string "00" ]);
  [%expect {| "[cases] match width is not equal to select width" |}];
  require_does_raise (fun () ->
    cases ~default:(of_string "000") (of_string "000") [ of_string "000", of_string "00" ]);
  [%expect {| ("[cases] got inputs of different widths" (000 00)) |}];
  require_does_raise (fun () ->
    cases
      ~default:(of_string "0000")
      (of_string "000")
      [ of_string "000", of_string "00" ]);
  [%expect {| ("[cases] got inputs of different widths" (0000 00)) |}];
  require_does_raise (fun () -> cases ~default:(of_string "00") (of_string "000") []);
  [%expect {| "[cases] no cases specified]" |}]
;;

let%expect_test "simulations" =
  let test ~select_width ~data_width =
    (* maximum number of cases, or lookups *)
    let max_num_cases = 100 in
    let max_num_lookups = 100 in
    let select = Signal.input "select" select_width in
    let num_cases = min max_num_cases (1 + Random.int (1 lsl select_width)) in
    let default = Signal.random ~width:data_width in
    let cases =
      List.init num_cases ~f:(fun _ ->
        Signal.random ~width:select_width, Signal.random ~width:data_width)
    in
    let q = Signal.cases ~default select cases in
    let q = Signal.output "q" q in
    let sim = Cyclesim.create (Circuit.create_exn ~name:"cases" [ q ]) in
    let select = Cyclesim.in_port sim "select" in
    let q = Cyclesim.out_port sim "q" in
    let to_bits s = Signal.to_constant s |> Bits.of_constant in
    let default = to_bits default in
    let cases =
      List.map cases ~f:(fun (match_with, value) -> to_bits match_with, to_bits value)
    in
    (* find the matching value *)
    let eval select =
      let rec find cases =
        match cases with
        | [] -> default
        | (match_with, value) :: _ when Bits.equal match_with select -> value
        | _ :: cases -> find cases
      in
      find cases
    in
    (* check all the matching cases *)
    let check select' =
      select := select';
      Cyclesim.cycle sim;
      if not (Bits.equal !q (eval select')) then raise_s [%message "BOOM"]
    in
    List.iter cases ~f:(fun (match_with, _) -> check match_with);
    (* random lookups *)
    let num_random_lookups = min max_num_lookups (1 lsl select_width) in
    for _ = 1 to num_random_lookups do
      check (Bits.random ~width:select_width)
    done
  in
  for _ = 1 to 100 do
    (* perform tests at various small and large sizes of the select and data width. *)
    test ~select_width:2 ~data_width:30;
    test ~select_width:10 ~data_width:100;
    test ~select_width:100 ~data_width:1;
    test ~select_width:165 ~data_width:77;
    test ~select_width:1000 ~data_width:2000
  done
;;
