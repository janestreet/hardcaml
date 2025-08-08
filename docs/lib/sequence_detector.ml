open Base
open Hardcaml
open Signal

module Explicit = struct
  (* $MDX part-begin=simple_101 *)
  module State = struct
    type t =
      | S1
      | S10
      | S101
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create ~clock ~clear ~d =
    let spec = Reg_spec.create ~clock ~clear () in
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            [ S1, [ if_ d [ sm.set_next S10 ] [ sm.set_next S1 ] ]
            ; S10, [ if_ d [ sm.set_next S1 ] [ sm.set_next S101 ] ]
            ; S101, [ sm.set_next S1 ]
            ]
        ]);
    reg spec (sm.is S101 &: d)
  ;;
  (* $MDX part-end *)
end

module Generic = struct
  (* $MDX part-begin=generic *)
  let next_states sequence cur_state =
    let sequence_length = Bits.width sequence in
    (cur_state + 1) % sequence_length, 0
  ;;

  let create ~sequence ~clock ~clear ~d =
    let sequence_length = Bits.width sequence in
    let module State = struct
      type t = int [@@deriving compare ~localize, sexp_of]

      (* Valid states are integers in the range [0..sequence_length-1]. *)
      let all = List.init sequence_length ~f:Fn.id
    end
    in
    let spec = Reg_spec.create ~clock ~clear () in
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            (List.init sequence_length ~f:(fun cur_state ->
               ( cur_state
               , let t, f = next_states sequence cur_state in
                 let t, f =
                   (* Swap the states if the match bit is [0]. *)
                   if Bits.to_bool sequence.Bits.:(cur_state) then t, f else f, t
                 in
                 [ if_ d [ sm.set_next t ] [ sm.set_next f ] ] )))
        ]);
    (* Decode the [detect] result - we are in the final state and the last bit matches. *)
    reg
      spec
      (sm.is (sequence_length - 1)
       &: if Bits.to_bool sequence.Bits.:(sequence_length - 1) then d else ~:d)
  ;;
  (* $MDX part-end *)
end

module Fixed = struct
  (* $MDX part-begin=fix_next_state *)
  let find_prev_match match_bit sequence cur_state =
    let open Bits in
    if cur_state = 0
    then 0
    else (
      let sequence = sequence.:[cur_state - 1, 0] in
      let rec find cur_state sequence match_ =
        if cur_state = 0
        then 0
        else if Bits.equal sequence match_
        then cur_state
        else if cur_state = 1
        then 0
        else find (cur_state - 1) (lsbs sequence) (msbs match_)
      in
      find cur_state sequence (msbs (match_bit @: sequence)))
  ;;

  let next_states sequence cur_state =
    let open Bits in
    let sequence_length = width sequence in
    let on_match =
      if cur_state = sequence_length - 1
      then find_prev_match sequence.:(cur_state) sequence cur_state
      else (cur_state + 1) % sequence_length
    in
    let no_match = find_prev_match ~:sequence.:(cur_state) sequence cur_state in
    on_match, no_match
  ;;

  (* $MDX part-end *)

  let%expect_test "test prev match search" =
    let open Bits in
    let test sequence =
      let length = width sequence in
      for cur_state = 0 to length - 1 do
        let prev_state = find_prev_match ~:sequence.:(cur_state) sequence cur_state in
        Stdio.print_s [%message (cur_state : int) (prev_state : int)]
      done
    in
    test (Bits.of_string "1101");
    [%expect
      {|
      ((cur_state 0) (prev_state 0))
      ((cur_state 1) (prev_state 1))
      ((cur_state 2) (prev_state 0))
      ((cur_state 3) (prev_state 2))
      |}];
    test (Bits.of_string "001101");
    [%expect
      {|
      ((cur_state 0) (prev_state 0))
      ((cur_state 1) (prev_state 1))
      ((cur_state 2) (prev_state 0))
      ((cur_state 3) (prev_state 2))
      ((cur_state 4) (prev_state 1))
      ((cur_state 5) (prev_state 3))
      |}]
  ;;

  let create ~sequence ~clock ~clear ~d =
    let sequence_length = Bits.width sequence in
    let module State = struct
      type t = int [@@deriving compare ~localize, sexp_of]

      let all = List.init sequence_length ~f:Fn.id
    end
    in
    let spec = Reg_spec.create ~clock ~clear () in
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            (List.init sequence_length ~f:(fun cur_state ->
               ( cur_state
               , let t, f = next_states sequence cur_state in
                 let t, f =
                   if Bits.to_bool sequence.Bits.:(cur_state) then t, f else f, t
                 in
                 [ if_ d [ sm.set_next t ] [ sm.set_next f ] ] )))
        ]);
    reg
      spec
      (sm.is (sequence_length - 1)
       &: if Bits.to_bool sequence.Bits.:(sequence_length - 1) then d else ~:d)
  ;;
end

(* $MDX part-begin=golden_model *)
let software_reference sequence bits =
  let open Bits in
  List.init
    (width bits - width sequence + 1)
    ~f:(fun pos ->
      if to_bool (bits.:[width sequence + pos - 1, pos] ==: sequence)
      then Some pos
      else None)
  |> List.filter_opt
  (* We find the first bit of the match, but the hardware will find the last bit so offset
     it by the length of the sequence. *)
  |> List.map ~f:(fun d -> d + width sequence - 1)
;;

(* $MDX part-end *)

let%expect_test "test reference" =
  let test sequence bits =
    let sequence = Bits.of_string sequence in
    let bits = Bits.of_string bits in
    let result = software_reference sequence bits in
    Stdio.print_s [%message (sequence : Bits.t) (bits : Bits.t) (result : int list)]
  in
  test "101" "101";
  test "101" "1010";
  test "101" "1011";
  test "110101" "1101010110001010110111101011001010101010100011101";
  [%expect
    {|
    ((sequence 101) (bits 101) (result (2)))
    ((sequence 101) (bits 1010) (result (3)))
    ((sequence 101) (bits 1011) (result (3)))
    ((sequence 110101) (bits 1101010110001010110111101011001010101010100011101)
     (result (27 48)))
    |}]
;;

open Fixed

let create_sequence_detector_sim ~sequence =
  Cyclesim.create
    (Circuit.create_exn
       ~name:"sequence_detected"
       [ output
           "detect"
           (create
              ~sequence
              ~clock:(input "clock" 1)
              ~clear:(input "clear" 1)
              ~d:(input "d" 1))
       ])
;;

(* $MDX part-begin=testbench *)
let test ~sequence ~data_in =
  let sim = create_sequence_detector_sim ~sequence in
  let d = Cyclesim.in_port sim "d" in
  let detect = Cyclesim.out_port sim "detect" in
  let open Bits in
  let cycles = width data_in in
  let results = ref [] in
  for i = 0 to cycles - 1 do
    d := data_in.:(i);
    Cyclesim.cycle sim;
    if Bits.to_bool !detect then results := i :: !results
  done;
  let results = List.rev !results in
  let expected = software_reference sequence data_in in
  if not (List.equal Int.equal results expected)
  then
    raise_s
      [%message
        (sequence : Bits.t) (data_in : Bits.t) (results : int list) (expected : int list)]
;;

(* $MDX part-end *)

(* $MDX part-begin=expect_test *)
let%expect_test "check random sequences" =
  for _ = 0 to 10 do
    let sequence = Bits.random ~width:(1 + Random.int 5) in
    for _ = 0 to 100 do
      let data_in = Bits.random ~width:(Bits.width sequence + Random.int 100) in
      test ~sequence ~data_in
    done
  done
;;
(* $MDX part-end *)
