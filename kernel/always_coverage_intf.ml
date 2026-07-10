open Coverage_prim

module type S = sig
  module Always_prim : Always_prim.S

  (** Attach always variable metadata to the provided signal *)
  val set_variable_metadata : Always_prim.Signal.t -> Always_metadata.Variable.t -> unit

  (** Compute Always if metadata and attach it to the provided signal *)
  val set_if_metadata
    :  Always_prim.Signal.t
    -> Always_prim.If_internal.t
    -> target:Always_prim.Variable.t
    -> unit

  (** Compute Always switch cases metadata and attach it to the provided signal *)
  val set_switch_cases_metadata
    :  Always_prim.Signal.t
    -> Always_prim.Switch_internal.t
    -> Always_prim.match_ list
    -> target:Always_prim.Variable.t
    -> unit

  (** Compute Always switch mux metadata and attach it to the provided signal *)
  val set_switch_mux_metadata
    :  Always_prim.Signal.t
    -> Always_prim.Switch_internal.t
    -> Always_prim.Match_internal.t
    -> target:Always_prim.Variable.t
    -> position:int
    -> unit

  (** If coverage is enabled and the target signal corresponds to and Always state machine
      state register, compute the valid state transitions for the given always statements
      and update the corresponding metadata on the target signal. *)
  val maybe_set_transitions_metadata
    :  Always_prim.t list
    -> target:Always_prim.Signal.t
    -> unit
end

module type Always_coverage = sig
  module type S = S

  module Make (Prim : Always_prim.S) : S with module Always_prim := Prim
end
