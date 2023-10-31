(** Schedules a circuit for a cyclesim simulation. *)

module Aliases : sig
  type t

  (** [Option.is_some (resolve_alias t x)] if the schedule determines that that [x]
      can be representated as a different signal w/o combinational loops without any
      computation. The is very benefitial in eliminating wires from "pure" computation
      and wires introduced from the Always DSL or [reg_fb].

      In such a scenario, [x] is a Wire, and does not need any combinational tasks to be
      compiled for it during simulation. Any operation that depends on [x] should depend
      directly on [resolve_alias t x].
  *)
  val resolve_alias : t -> Signal.Uid.t -> Signal.Uid.t option

  (** Returns true iff [Option.is_none (resolve_alias t x)]. *)
  val is_alias : t -> Signal.Uid.t -> bool
end

type t

(** Sorted list of combinational nodes that needs to be evaluated before/after
    at a clock edge. *)
val schedule : t -> Signal.t list

(** Sorted list of registers to be updated at a clock edge. *)
val regs : t -> Signal.t list

(** Memories in the circuit. Memories should be updated after [regs]
    are updated on the clock edge. The order of memory updates do not matter.
*)
val mems : t -> Signal.t list

(** Constant nodes. *)
val consts : t -> Signal.t list

(** Input nodes. *)
val inputs : t -> Signal.t list

(** Returns information about alias nodes. *)
val aliases : t -> Aliases.t

val create : Circuit.t -> t
