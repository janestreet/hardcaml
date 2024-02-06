(** Assertions within Hardcaml simulations.

    There are a few steps needed to enable assertions:

    1. pass [trace_properties:true] to [Scope.create]
    2. enable them in the [Circuit.Config] (see [circuit_config_with_assertions])
    3. (optionally) attach assertion tracing to the simulation (see [trace])
*)

open! Base

type t [@@deriving sexp_of]

module Violated_or_not : sig
  type t =
    | Violated of int list
    | Not_violated
  [@@deriving sexp_of]
end

(** Return a circuit config with assertions enabled. *)
val circuit_config_with_assertions
  :  ?config:Circuit.Config.t
  -> Scope.t
  -> Circuit.Config.t

(** Connect to a simulator and record all assertions. *)
val trace : ('i, 'o) Cyclesim.t -> Assertion_manager.t option -> t * ('i, 'o) Cyclesim.t

(** Return assertions fired during a simulation. Printing [t] as a sexp displays these
    results. *)
val results : t -> Violated_or_not.t Map.M(String).t option

(** Add an assertion to the scope's assertion manager. *)
val add : Scope.t -> string -> Signal.t -> unit

module Always : sig
  (** Add an assertion, fired within the context of an always block. *)
  val add : Scope.t -> string -> Signal.t -> Always.t
end
