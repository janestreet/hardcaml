open Base

type t [@@deriving sexp_of]

val create : unit -> t

(** Add a named LTL property *)
val add_ltl : t -> string -> Property.LTL.path -> unit

(** Disables adding new properties, and returns a map of the LTL
    properties added. Calling it again will return the map again *)
val finalize : t -> Property.LTL.path Map.M(String).t

(** Finalizes the [Property_manager] if not already finalized, and
    return a list of all [Signal.t] used as atomic propositions.
    This list will not contain duplicates *)
val atomic_propositions : t -> Signal.t list
