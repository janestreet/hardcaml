(** [Clocked_signal.t] is similar to a [Signal.t], but with clock domain info attached to
    it. *)

open! Core0

type t

include Signal.S with type t := t (** @inline *)

(** Check if the provided signals are consistent where the definition of consistency is
    defined by the underlying signal type.

    For the unclocked Signal.t, this does nothing.

    For the clocked signal, this asserts that all the provided signals have the same
    runtime domain and return the domain, raises if they're not compatible. [~op_name]
    will be shown in the error message reporting a clock domain mismatch.

    The rules of runtime clock domain consistencies are:
    - There cannot be any unknowns.
    - Amongst the non-constants, their clock domains must match.
    - Having only constants (or an empty list) is valid, in which case this function will
      just return a constant runtime clock domain. *)
val validate_signals_are_consistent
  :  op_name:string
  -> (string * t) list
  -> Clock_domain.Runtime.t

(** The elaboration-time runtime clock domain of the signal. *)
val get_domain : t -> Clock_domain.Runtime.t

(** Validates that when [required_domain] is expected, [dom t] is able to satisfy that
    requirement.

    - When [required_domain] is a constant, [dom t] have to be a constant
    - When [required_domain] is [Exact x], [dom t] can be either [Exact x] or [Constant]
    - When [required_domain] is [Unknown], this check will always fail. *)
val ensure_domain
  :  loc:[%call_pos]
  -> ?name:string
  -> t
  -> required_domain:Clock_domain.Runtime.t
  -> t

(** Return the base signal if the clocked representation has the given domain. *)
val unwrap_signal : loc:[%call_pos] -> t -> dom:Clock_domain.Runtime.t -> Signal.t

(** Creates a signal with the associated domain. Raises if the signal already has a domain
    attached to it. *)
val to_clocked : Signal.t -> dom:Clock_domain.Runtime.t -> t

(** Map over the base implementation, keeping the same info. *)
val map : t -> f:(Signal.t -> Signal.t) -> t

module Unsafe : sig
  (** Change the domain of the signal. The caller must guarantee that this transformation
      is safe. *)
  val set_domain : t -> dom:Clock_domain.Runtime.t -> t

  (** Crossing between synchronous domains. The caller must ensure that the two domains
      are related. *)
  val sync_crossing
    :  loc:[%call_pos]
    -> from_dom:Clock_domain.Runtime.t
    -> to_dom:Clock_domain.Runtime.t
    -> t
    -> t

  (** Convert a clocked [Reg_spec.t] to an unclocked one. *)
  val reg_spec_to_unclocked : Reg_spec.t -> Signal.Reg_spec.t
end

(** Defines functions that shadow the standard [Signal] construct functions and ensure
    that a domain is provided. *)
module Overrides : sig
  (** Construct a wire in a particular clock domain. Meant to replace uses of
      [Signal.wire] which does not come with a clock domain. *)
  val wire : int -> dom:Clock_domain.Runtime.t -> t

  (** Construct an input signal in a particular clock domain. Meant to replace uses of
      [Signal.input] which does not come with a clock domain. *)
  val input : string -> int -> dom:Clock_domain.Runtime.t -> t
end

module Expert : sig
  include module type of Expert

  val validate_assign_in_always : dst:t -> src:t -> unit

  val reg__with_signal_reset
    :  ?enable:t
    -> ?initialize_to:Bits.t
    -> ?reset_to:t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> t
    -> t
end
