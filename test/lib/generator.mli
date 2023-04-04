(** This module generates random Hardcaml circuits. Intended for testing simulators and
    circuit graph transformations. *)

(** Generates a random circuit with maximum depth [depth].
    The circuit won't have any inputs unless [allow_inputs] is true. *)
val gen_circuit
  :  allow_inputs:bool
  -> depth:int
  -> Hardcaml.Circuit.t Core.Quickcheck.Generator.t

(** Generates input data for a given circuit. *)
val gen_input_data
  :  Hardcaml.Circuit.t
  -> (string * Hardcaml.Bits.t) list Core.Quickcheck.Generator.t

(** Generates a random signal, with children being other random signals and signals from
    [inputs]. *)
val gen
  :  width:int
  -> depth:int
  -> inputs:Hardcaml.Signal.t list
  -> Hardcaml.Signal.t Core.Quickcheck.Generator.t
