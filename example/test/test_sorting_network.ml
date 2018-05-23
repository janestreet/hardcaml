open! Import
open! Sorting_network

let%expect_test "power of 2 length" =
  require_does_raise [%here] (fun () ->
    create Bitonic_sort (fun _ _ -> assert false) [ 1; 2; 3 ]);
  [%expect {|
    ("Sorting networks require their input length to be a power of 2"
     (config Bitonic_sort)
     (length 3)) |}]
;;

let%expect_test "sort integers" =
  let inputs = List.init 16 ~f:(fun _ -> Random.int 100) in
  let sorted =
    create Bitonic_sort
      (fun a b ->
         if a < b
         then { min = a; max = b }
         else { min = b; max = a })
      inputs in
  print_s [%message "" (inputs : int list) (sorted : int list)];
  [%expect {|
    ((inputs (3 91 20 36 55 16 88 49 3 22 63 83 90 68 32 74))
     (sorted (3 3 16 20 22 32 36 49 55 63 68 74 83 88 90 91))) |}]

let sort_ascending_unsigned : Bits.t compare_and_swap =
  (fun a b ->
     let open Bits in
     let sel = a <: b in
     { min = mux2 sel a b; max = mux2 sel b a })

let%expect_test "bitonic, unsigned, ascending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.srand 4) in
  let sorted = create Bitonic_sort sort_ascending_unsigned inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect {|
    ((inputs (0010 0100 1100 1110))
     (sorted (0010 0100 1100 1110))) |}]

let sort_descending_signed : Bits.t compare_and_swap =
  (fun a b ->
     let open Bits in
     let sel = a <+ b in
     { min = mux2 sel b a; max = mux2 sel a b })

let%expect_test "bitonic, signed, descending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.srand 4) in
  let sorted = create Bitonic_sort sort_descending_signed inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect {|
    ((inputs (0010 0100 1100 1110))
     (sorted (0100 0010 1110 1100))) |}]

let%expect_test "odd_even_merge, unsigned, ascending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.srand 4) in
  let sorted = create Odd_even_merge_sort sort_ascending_unsigned inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect {|
    ((inputs (0010 0100 1100 1110))
     (sorted (0010 0100 1100 1110))) |}]

let%expect_test "odd_even_merge, signed, descending" =
  let inputs = List.init 4 ~f:(fun _ -> Bits.srand 4) in
  let sorted = create Odd_even_merge_sort sort_descending_signed inputs in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect {|
    ((inputs (0010 0100 1100 1110))
     (sorted (0100 0010 1110 1100))) |}]

let%expect_test "sort by bottom 2 bits" =
  let inputs = List.init 8 ~f:(fun _ -> Bits.srand 4) in
  let sorted =
    create Bitonic_sort
      (fun a b ->
         let open Bits in
         let sel = select a 1 0 <: select b 1 0 in
         { min = mux2 sel a b; max = mux2 sel b a })
      inputs
  in
  print_s [%message "" (inputs : Bits.t list) (sorted : Bits.t list)];
  [%expect {|
    ((inputs (0011 0110 0111 0111 0010 0100 1100 1110))
     (sorted (1100 0100 0010 1110 0110 0111 0111 0011))) |}]

let%expect_test "check all possible zero-one inputs" =
  List.iter Config.all ~f:(fun config ->
    List.iter [ 1; 2; 4; 8; 16 ] ~f:(fun num_inputs ->
      for i = 0 to Int.pow 2 num_inputs - 1 do
        let inputs = Bits.bits (Bits.consti num_inputs i) in
        let sorted = create config sort_ascending_unsigned inputs in
        require [%here]
          (List.is_sorted sorted
             ~compare:(fun b1 b2 ->
               if Bits.equal (Bits.( <: ) b1 b2) Bits.vdd
               then -1
               else if Bits.equal b1 b2
               then 0
               else 1))
      done));
  [%expect {| |}];
;;
