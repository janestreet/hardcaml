(** Coverage information about a single circuit.

    Circuit instantiations are considered equal, and coverage is merged, when they are
    structurally equal. *)

open! Core0

module Instance : sig
  type t = { call_stack : Call_stack.t @@ global }
end

type t

val create : Circuit.t -> t
val add_if_structurally_equal : t -> Circuit.t -> bool

(** Signal coverage is uniquely identified in a circuit by a signal's normalized signal
    id. Return the coverage if it already exists, or create one if coverage is supported
    for the signal type. *)
val maybe_find_or_create_signal_coverage
  :  t
  -> Signal.t
  -> Signal_graph.Normalized_signal_uid.t
  -> Signal_coverage.t option

val name : t -> string
val instance_counts : t -> (Instance.t * int) list

(** List of signal coverage associated with this circuit. *)
val signal_coverage : t -> Signal_coverage.t list

include Coverage.S with type t := t
