(** Assertions within Hardcaml simulations.

    There are a few steps needed to enable assertions:

    1. pass [trace_properties:true] to [Scope.create]
    2. Add the property manager from the scope to [Scope.circuit_config_with_properties]
    3. (optionally) attach assertion tracing to the simulation (see [trace]) *)

open! Core0

type t [@@deriving sexp_of]

module Violated_or_not : sig
  type t =
    | Violated of int list
    | Not_violated
  [@@deriving sexp_of]
end

(** Connect to a simulator and record all assertions. *)
val trace : ('i, 'o) Cyclesim.t -> Property_manager.t option -> t * ('i, 'o) Cyclesim.t

(** Return assertions fired during a simulation. Printing [t] as a sexp displays these
    results. *)
val results : t -> Violated_or_not.t Hashtbl.M(String).t option

(** Add an assertion to the scope's assertion manager. *)
val add : Scope.t -> string -> Signal.t -> unit

module Always : sig
  (** Add an assertion, fired within the context of an always block. *)
  val add : Scope.t -> string -> Signal.t -> Always.t
end
