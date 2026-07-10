(** [Clock_domain] contains functions to work with clock domains. *)

open! Core0

(** An exact non-constant known clock domain. *)
module Exact : sig
  module Uid : Uid_builder.S

  type t [@@deriving sexp_of]

  include Equal.S with type t := t

  (** Get the name associated with the runtime clock domain. *)
  val get_name : t -> string

  module Expert : sig
    (** Resets the UID. Meant for expert use only -- we rely on the fact that UID equality
        to hold iff the runtime clock domains hold. *)
    val reset_uid : unit -> unit

    (** Useful to create domains with specific names for testing. *)
    val create_domain : loc:[%call_pos] -> string -> t
  end
end

(** The runtime clock domain associated to a [Clock_domain.t] *)
module Runtime : sig
  type t =
    | Constant
    (** A signal with an [Constant] clock domain is one that is compatible with any clock
        domain requirement. Constants falls under this category. *)
    | Unknown
    (** A signal with an unknown domain is one which isn't assigned. Most of the functions
        in [Clocked_signal.t] will reject an unknown domain.

        This is usually constructed from functions like [Clock_signal.wire], which doesn't
        have a clock domain specified by default. *)
    | Exact of Exact.t
    (** A signal with an [Exact] clock domain is one that is mapped to a specific clock
        domain. *)
  [@@deriving sexp_of, equal ~localize]

  val get_name : t -> string
  val exact_exn : t -> Exact.t
end

module Uid : Uid_builder.S

type t [@@deriving sexp_of, equal]

(** Obtain the name of the clock domain. Note that the name is only for informational
    purposes. Clock domain equality is identified via the uid.

    Specifically, two clock domains created via [Clock_comain.create] with the same string
    argument, will _not_ have the same clock domain. *)
val get_name : t -> string

val create : loc:[%call_pos] -> string -> t
val uid : t -> Uid.t
val dedup_by_uid : t list -> t list

(** Generate a new fresh runtime clock domain value from a given specification value.
    Calling this function will always generate a new fresh domain. *)
val generate_fresh_exact_domain : loc:[%call_pos] -> t -> Exact.t

type construct_mapping_error =
  | Maps_to_unknown
  | Maps_to_multiple of Exact.t list
[@@deriving sexp_of]

type maps_to =
  | Exact of Exact.t
  | All_constants
[@@deriving sexp_of]

type mapped_clock_domain =
  { specification_domain : t
  ; maps_to : (maps_to, construct_mapping_error) Result.t
  }
[@@deriving sexp_of]

(** Construct a mapping from a specification domain to an exact domain. The semantics are:

    - If a [t] has only got constants associated to it, it will not be present in the
      mapping
    - If a [t] has any unknowns associated to it, it will get mapped to
      [Error Maps_to_unknown]
    - If a [t] has more than 1 unique runtime clock domain associated to it, it will get
      mapped to [Error (Maps_to_multiple _)]
    - Otherwise, it will be [Ok exact_domain] *)
val construct_mapping_from_spec_to_exact
  :  (t * Runtime.t) list
  -> mapped_clock_domain Uid.Map.t

module Expert : sig
  val create_with_uid : loc:[%call_pos] -> string -> Uid.t -> t
end
