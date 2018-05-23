open! Import
open! Lfsr

let run_lfsr ~nbits ~verbose ~config ~counterpart_taps ~op =
  let nstates = (1 lsl nbits) - 1 in
  let lfsr state = create ~config ~counterpart_taps ~op (module Bits) state in
  let rec loop n state set =
    if n = nstates
    then state, set
    else
      let state = lfsr state in
      let set = Set.add set (Bits.to_int state) in
      if verbose then print_s [%message "" (state : Bits.t)];
      loop (n+1) state set
  in
  let start =
    match op with
    | Xnor -> Bits.zero nbits
    | Xor  -> Bits.ones nbits in
  let end_, set = loop 0 start (Set.empty (module Int)) in
  print_s [%message "" ~num_computed_states:(Set.length set : int)];
  if not (Bits.equal start end_)
  then print_s [%message "Start state does not equal end state"];
  if Set.length set <> nstates
  then print_s [%message "LFSR did not generate all possible states"]

(* Test all variants of a 3 bit LFSR.  We should see all 7 possible states (note; not 8!)
   and finish in the same state we started in. *)
let%expect_test "3 bit galois, xor" =
  List.iter Config.all ~f:(fun config ->
    List.iter Bool.all ~f:(fun counterpart_taps ->
      List.iter Op.all ~f:(fun op ->
        print_s [%message
          ""
            (config : Config.t)
            (counterpart_taps : bool)
            (op : Op.t)];
        run_lfsr ~nbits:3 ~verbose:true ~config ~counterpart_taps ~op)));
  [%expect {|
    ((config           Galois)
     (counterpart_taps false)
     (op               Xor))
    (state 101)
    (state 100)
    (state 010)
    (state 001)
    (state 110)
    (state 011)
    (state 111)
    (num_computed_states 7)
    ((config           Galois)
     (counterpart_taps false)
     (op               Xnor))
    (state 010)
    (state 011)
    (state 101)
    (state 110)
    (state 001)
    (state 100)
    (state 000)
    (num_computed_states 7)
    ((config           Galois)
     (counterpart_taps true)
     (op               Xor))
    (state 110)
    (state 011)
    (state 100)
    (state 010)
    (state 001)
    (state 101)
    (state 111)
    (num_computed_states 7)
    ((config           Galois)
     (counterpart_taps true)
     (op               Xnor))
    (state 001)
    (state 100)
    (state 011)
    (state 101)
    (state 110)
    (state 010)
    (state 000)
    (num_computed_states 7)
    ((config           Fibonacci)
     (counterpart_taps false)
     (op               Xor))
    (state 110)
    (state 100)
    (state 001)
    (state 010)
    (state 101)
    (state 011)
    (state 111)
    (num_computed_states 7)
    ((config           Fibonacci)
     (counterpart_taps false)
     (op               Xnor))
    (state 001)
    (state 011)
    (state 110)
    (state 101)
    (state 010)
    (state 100)
    (state 000)
    (num_computed_states 7)
    ((config           Fibonacci)
     (counterpart_taps true)
     (op               Xor))
    (state 110)
    (state 101)
    (state 010)
    (state 100)
    (state 001)
    (state 011)
    (state 111)
    (num_computed_states 7)
    ((config           Fibonacci)
     (counterpart_taps true)
     (op               Xnor))
    (state 001)
    (state 010)
    (state 101)
    (state 011)
    (state 110)
    (state 100)
    (state 000)
    (num_computed_states 7) |}];
;;

(* Test a few different sizes, random configs, but nothing too big *)
let%expect_test "2 bit" =
  run_lfsr ~nbits:2 ~verbose:false ~config:Fibonacci ~counterpart_taps:false ~op:Xor;
  [%expect {| (num_computed_states 3) |}]

let%expect_test "6 bit" =
  run_lfsr ~nbits:6 ~verbose:false ~config:Galois ~counterpart_taps:false ~op:Xor;
  [%expect {| (num_computed_states 63) |}]

let%expect_test "9 bit" =
  run_lfsr ~nbits:9 ~verbose:false ~config:Galois ~counterpart_taps:true ~op:Xor;
  [%expect {| (num_computed_states 511) |}]

let%expect_test "13 bit" =
  run_lfsr ~nbits:13 ~verbose:false ~config:Fibonacci ~counterpart_taps:false ~op:Xnor;
  [%expect {| (num_computed_states 8191) |}]

let%expect_test "invalid length exns" =
  show_raise (fun () ->
    run_lfsr ~nbits:1 ~verbose:false ~config:Galois ~counterpart_taps:false ~op:Xor);
  [%expect {| (raised ("LFSR length must be >=2 and <= 168" (length 1))) |}];
  show_raise (fun () ->
    run_lfsr ~nbits:169 ~verbose:false ~config:Galois ~counterpart_taps:false ~op:Xor);
  [%expect {| (raised ("LFSR length must be >=2 and <= 168" (length 169))) |}]

let%expect_test "all supported lengths work" =
  for length=2 to 168 do
    require_does_not_raise [%here] (fun () ->
      ignore (create (module Bits) (Bits.zero length) : Bits.t))
  done;
  [%expect {||}]
