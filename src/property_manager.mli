open! Core0

(** A group of properties. Properties are tagged with a group when added, and groups can
    be selectively enabled when tracing (see {!Trace_properties}). *)
module Group : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  (** The (human readable) name the group was created with. *)
  val name : t -> string

  (** Create a new group with the given (human readable) name. Each call returns a fresh,
      distinct group. *)
  val create : string -> t

  (** The catch-all group that properties are associated with when no group is specified. *)
  val default : t

  (** Singleton property group used to represent contracts between module interfaces. *)
  val contract : t

  (** Group which represents every property. *)
  val all : t
end

(** Specifies which kinds of properties, and which groups, should be traced. *)
module Trace_properties : sig
  type t =
    { enable_sva : bool
    ; enable_ltl : bool
    ; enable_assertions : bool
    ; enabled_property_groups : Group.t list
    }
  [@@deriving sexp_of]

  (** Trace nothing. *)
  val none : t

  (** Trace every property that has been defined, regardless of its group (enables the
      special {!Group.all} group). *)
  val all : t

  (** Trace all property types within the {!Group.default} group. *)
  val default : t

  (** Each property type defaults to disabled, and [groups] defaults to [[Group.default]]. *)
  val create
    :  ?enable_sva:bool
    -> ?enable_ltl:bool
    -> ?enable_assertions:bool
    -> ?groups:Group.t list
    -> unit
    -> t

  (** True if any property type is enabled. *)
  val any_enabled : t -> bool

  (** True if any property type used in simulation (assertions or ltl) is enabled. *)
  val for_simulation : t -> bool
end

type t [@@deriving sexp_of]

val create : unit -> t

(** Return a new manager containing only the properties whose type is enabled and whose
    group is one of the enabled groups in [trace_properties]. *)
val filter : t -> Trace_properties.t -> t

(** {2 Simple assertions} *)

val add_assertion : ?group:Group.t -> t -> string -> Signal.t -> unit
val get_assertions : t -> (string, Signal.t) Hashtbl.t

(** {2 LTL properties} *)

(** Add a named LTL property *)
val add_ltl : ?group:Group.t -> t -> string -> Property.LTL.t -> unit

(** Returns a set of [Signal.t]s used as atomic propositions. *)
val get_ltl_atomic_propositions : t -> Set.M(Signal.Type.Signal_compared_by_uid).t

(** LTL properties *)
val get_ltl_properties : t -> (string, Property.LTL.t) Hashtbl.t

(** {2 SVA properties} *)

(** Add a named LTL property *)
val add_sva : ?group:Group.t -> t -> string -> Signal.t Property.Sva.t -> unit

(** Returns a set of all [Signal.t] used as atomic propositions. *)
val get_sva_atomic_propositions : t -> Set.M(Signal.Type.Signal_compared_by_uid).t

(** Returns a set of all [Signal.t] used in properties, including those as part of
    [Clocked.t]. *)
val get_sva_signals : t -> Set.M(Signal.Type.Signal_compared_by_uid).t

(** SVA properties *)
val get_sva_properties : t -> (string, Signal.t Property.Sva.t) Hashtbl.t

(** Rewrite the sva properties and return a new manager. *)
val map_sva_properties : t -> f:(Signal.t Property.Sva.t -> Signal.t Property.Sva.t) -> t
