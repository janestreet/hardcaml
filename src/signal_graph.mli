(** A [Signal_graph.t] is a created from a list of signals, and defined by tracing back to
    inputs (unassigned wires or constants). Functions are provided for traversing the
    graph.

    When traversing using [depth_first_seach] we can specify [upto] which means the search
    will stop when it reaches any signal in the given list (which is also /not/ included
    as part of the search). *)

open! Core0

module Normalized_signal_uid : sig
  type t = private Signal.Type.Uid.t
  [@@deriving compare ~localize, hash, sexp_of, to_string]

  include Comparator.S with type t := t

  include%template Equal.S [@mode local] with type t := t
end

type t [@@deriving sexp_of]

(** Create a [Signal_graph.t] from a list of signals (commonly, circuit outputs). *)
val create : ?upto:Signal.t list -> Signal.t list -> t

(** Traverse the graph and find all inputs. Badly formed inputs (no name, or multiple
    names) return an error. *)
val inputs : t -> Signal.t list Or_error.t

(** Checks the outputs of the signal graph for compatibility with circuit outputs. *)
val validate_outputs : t -> unit Or_error.t

(** Return the outputs of the signal graph. *)
val outputs : t -> Signal.t list

(** Visit all signals in the graph, starting at the outputs, in a depth-first manner. Each
    signal is visited only once. [f_before] is called before recursing on each signal's
    fan-in. Similiarly, [f_after] is called after recursing on the fan-in.

    If [deps] is provided it will be used to compute signal dependencies rather than the
    default definition. This is useful for terminating traversals based on some condition
    on signals, e.g., if it's a register or a memory. *)
val depth_first_search
  :  ?deps:(module Signal.Type.Deps)
  -> ?f_before:('a -> Signal.t -> 'a)
  -> ?f_after:('a -> Signal.t -> 'a)
  -> t
  -> init:'a
  -> 'a

(** Fold across all signals in the graph, starting at the outputs. Each signal is visited
    only once. *)
val fold
  :  ?deps:(module Signal.Type.Deps)
  -> t
  -> init:'a
  -> f:('a -> Signal.t -> 'a)
  -> 'a

(** Return a list of all signals in the graph for whom [f signal] returns true. *)
val filter : ?deps:(module Signal.Type.Deps) -> t -> f:(Signal.t -> bool) -> Signal.t list

(** Iterate over all signals in the graph. *)
val iter : ?deps:(module Signal.Type.Deps) -> t -> f:(Signal.t -> unit) -> unit

(** Retuns an error if the graph has a combinational loop, that is, a path from a signal
    back to itself that doesn't pass though a register, memory or instantiation. *)
val detect_combinational_loops : t -> unit Or_error.t

(** Rewrites [t] by creating a copy of every signal in [t] and applying [f] to each signal
    and [f_upto] to all of the uptos of the signal graph. Also returns a map from the uid
    of all of the rewritten signals to their corresponding new signal. *)
val rewrite
  :  t
  -> f:(Signal.t -> Signal.t)
  -> f_upto:(Signal.t -> Signal.t)
  -> t * Signal.t Map.M(Signal.Type.Uid).t

(** [normalize_uids t] creates a copy of [t] that is identical to [t] except the uids are
    numbered starting at 1. *)
val normalize_uids : t -> t

(** Creates a mapping from current uids to normalized uids (starting from 1), without
    modifying the current graph. *)
val compute_normalized_uids : t -> (Normalized_signal_uid.t * Signal.t) list

(** Fan-out of each signal in the signal graph. The fan-out of a signal is the set of
    signals it drives. *)
val fan_out_map : t -> Signal.Type.Uid_set.t Map.M(Signal.Type.Uid).t

(** Fan-in of each signal in the signal graph. The fan-in of a signal is the set of
    signals that drive it. *)
val fan_in_map : t -> Signal.Type.Uid_set.t Map.M(Signal.Type.Uid).t

(** [topological_sort t] sorts the signals in [t] so that all the signals in [deps s]
    occur before [s]. *)
val topological_sort
  :  deps:(module Signal.Type.Deps)
  -> t
  -> (Signal.t list, Signal.t list) Result.t

val topological_sort_exn : deps:(module Signal.Type.Deps) -> t -> Signal.t list

(** Map all clock signals to their driving input signals. This mapping can be used to
    identify clock domains of Registers and Memories. *)
val resolve_clock_domains : t -> Signal.t Map.M(Signal.Type.Uid).t

(** For rtl generation the case matches are written explicitly and do not need to be
    tracked. *)
module Deps_without_case_matches : Signal.Type.Deps

(** Signal dependencies used for simulation scheduling. Breaks loops through sequential
    elements like registers and memories. *)
module Deps_for_simulation_scheduling : Signal.Type.Deps

(** Like [Deps_for_simulation_scheduling], except loops are allowed through
    instantiations. *)
module Deps_for_loop_checking : Signal.Type.Deps

(** Final layer of combinational nodes which sit on the path between the outputs and any
    driving register or memory. *)
val last_layer_of_nodes : is_input:(Signal.t -> bool) -> t -> Signal.Type.Uid.t List.t
