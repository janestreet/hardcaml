open Base
open Hardcaml
open Signal
open Hardcaml_waveterm

(* $MDX part-begin=interface *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; n : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { done_ : 'a [@rtlname "done"]
    ; result : 'a [@bits 32]
    ; state : 'a [@bits 2]
    }
  [@@deriving hardcaml]
end
(* $MDX part-end *)

(* $MDX part-begin=states *)
module States = struct
  type t =
    | S_wait
    | S_counting
    | S_write_result
  [@@deriving sexp_of, compare, enumerate]
end
(* $MDX part-end *)

(* $MDX part-begin=implementation *)
let create (i : _ I.t) =
  let r_sync = Signal.Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
  let done_ = Always.Variable.wire ~default:gnd in
  let result = Always.Variable.wire ~default:(zero 32) in
  let f0 = Always.Variable.reg ~width:32 ~enable:Signal.vdd r_sync in
  let f1 = Always.Variable.reg ~width:32 ~enable:Signal.vdd r_sync in
  let remaining = Always.Variable.reg ~width:8 ~enable:Signal.vdd r_sync in
  Always.(
    compile
      [ sm.switch
          [ ( S_wait
            , [ (* The [a <--. b] is semantically equivalent to

                   [a <-- Signal.of_unsigned_int ~width:(Signal.width a.value) b]

                   Similar syntactic sugar exists for [+:.] and [-:.]. *)
                f0 <--. 1
              ; f1 <--. 1
              ; remaining <-- i.n -:. 1
              ; when_
                  i.start
                  [ if_
                      (i.n ==:. 0)
                      [ sm.set_next S_write_result ]
                      [ sm.set_next S_counting ]
                  ]
              ] )
          ; ( S_counting
            , [ if_
                  (remaining.value ==:. 0)
                  [ sm.set_next S_write_result ]
                  [ remaining <-- remaining.value -:. 1
                  ; (* Recall that all hardcaml assignments are synchronous, so the [f1]
                       assignment below will make use of the old [f0] value. *)
                    f0 <-- f1.value
                  ; f1 <-- f0.value +: f1.value
                  ]
              ] )
          ; (* Output the computation result to the user. *)
            S_write_result, [ done_ <--. 1; result <-- f1.value; sm.set_next S_wait ]
          ]
      ]);
  { O.done_ = done_.value
  ; result = result.value
  ; state =
      (* We output the [state] to help tracing in the simulation example that follows. *)
      sm.current
  }
;;

(* $MDX part-end *)

(* $MDX part-begin=testbench *)
let fibonacci_testbench (sim : (_ I.t, _ O.t) Cyclesim.t) =
  let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
  let print_state_and_outputs () =
    let state = List.nth_exn States.all (Bits.to_unsigned_int !(outputs.state)) in
    let done_ = Bits.to_bool !(outputs.done_) in
    let result = Bits.to_unsigned_int !(outputs.result) in
    Stdio.print_s [%message (state : States.t) (done_ : bool) (result : int)]
  in
  (* Start by resetting simulation and clearing the circuit. *)
  Cyclesim.reset sim;
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;
  (* Cycle 0 *)
  print_state_and_outputs ();
  (* Cycle 1 *)
  inputs.start := Bits.vdd;
  inputs.n := Bits.of_unsigned_int ~width:8 4;
  Cyclesim.cycle sim;
  print_state_and_outputs ();
  inputs.start := Bits.gnd;
  (* Cycle 2 - Start counting the fibonacci number. *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();
  (* Cycle 3 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();
  (* Cycle 4 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();
  (* Cycle 5 - notice that the wire assignment is combinational - the [done] signal is
     asserted during the same cycle it is assigned in the state machine. *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();
  (* Cycle 6 - this goes back to the initial waiting state. *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();
  Cyclesim.cycle sim
;;

let test () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim = Sim.create create in
  let waves, sim = Waveform.create sim in
  fibonacci_testbench sim;
  waves
;;

(* $MDX part-end *)

(* $MDX part-begin=display_rules *)
let display_rules =
  let open Display_rule in
  let input_rules =
    I.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)) |> to_list)
  in
  let output_rules =
    { O.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int))) with
      O.state =
        port_name_is
          "state"
          ~wave_format:
            (Index
               (List.map States.all ~f:(fun t -> States.sexp_of_t t |> Sexp.to_string)))
    }
    |> O.to_list
  in
  input_rules @ output_rules
;;
(* $MDX part-end *)
