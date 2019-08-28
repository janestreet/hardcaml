(** A [Signal_graph.t] is a created from a list of signals, and defined by tracing back
    to inputs (unassigned wires or constants).  Functions are provided for traversing
    the graph. *)

open! Import

type t [@@deriving sexp_of]

(** Create a [Signal_graph.t] from a list of signals (commonly, circuit outputs). *)
val create : Signal.t list -> t

(** Traverse the graph and find all inputs.  Badly formed inputs (no name, or multiple
    names) return an error. *)
val inputs : t -> Signal.t list Or_error.t

(** Return the outputs of the signal graph.  If [validate] is [true], then the outputs
    are checked for compatibility with circuit outputs. *)
val outputs : ?validate:bool (** default is [false] *) -> t -> Signal.t list Or_error.t

(** Visit all signals in the graph, starting at the outputs, in a depth-first manner.
    Each signal is visited only once.  [f_before] is called before recursing on each
    signal's fan-in.  Similiarly, [f_after] is called after recursing on the fan-in.

    If [deps] is provided it will be used to compute signal dependencies rather
    than the default definition. This is useful for terminating traversals
    based on some condition on signals, e.g., if it's a register or a memory. *)
val depth_first_search
  :  ?deps:(Signal.t -> Signal.t list)
  -> ?f_before:('a -> Signal.t -> 'a)
  -> ?f_after:('a -> Signal.t -> 'a)
  -> t
  -> init:'a
  -> 'a

(** Fold across all signals in the graph, starting at the outputs.  Each signal is visited
    only once. *)
val fold : t -> init:'a -> f:('a -> Signal.t -> 'a) -> 'a

(** Return a list of all signals in the graph for whom [f signal] returns true. *)
val filter : t -> f:(Signal.t -> bool) -> Signal.t list

(** Iterate over all signals in the graph. *)
val iter : t -> f:(Signal.t -> unit) -> unit

(** Retuns an error if the graph has a combinational loop, that is, a path from a signal
    back to itself that doesn't pass though a register, memory or instantiation. *)
val detect_combinational_loops : t -> unit Or_error.t

(** [normalize_uids t] creates a copy of [t] that is identical to [t] except the
    uids are numbered starting at 1. *)
val normalize_uids : t -> t

(** Fan-out of each signal in the signal graph.  The fan-out of a signal is the set of
    signals it drives.*)
val fan_out_map
  :  ?deps:(Signal.t -> Signal.t list)
  -> t
  -> Signal.Uid_set.t Signal.Uid_map.t

(** Fan-in of each signal in the signal graph.  The fan-in of a signal is the set of
    signals that drive it.*)
val fan_in_map
  :  ?deps:(Signal.t -> Signal.t list)
  -> t
  -> Signal.Uid_set.t Signal.Uid_map.t

(** [topological_sort t] sorts the signals in [t] so that all the signals in [deps s]
    occur before [s]. *)
val topological_sort : ?deps:(Signal.t -> Signal.t list) -> t -> Signal.t list

(** Signal dependencies used for scheduling. Breaks loops through sequential elements like
    registers and memories. *)
val scheduling_deps : Signal.t -> Signal.t list

(** Final layer of combinational nodes which sit on the path between the outputs and any
    driving register or memory. *)
val last_layer_of_nodes : is_input:(Signal.t -> bool) -> t -> Signal.Uid.t List.t
