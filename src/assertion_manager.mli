open Base

type t [@@deriving sexp_of]

(* Disables adding new assertions, and returns a map of the assertions added.
   Calling it again will return the map. *)
val finalize : t -> Signal.t Map.M(String).t
val create : unit -> t

(* Add a named assertion *)
val add : t -> string -> Signal.t -> unit

(** Create a finalized assertion manager instance that contains the provided
    assertions. *)
val of_signals : Signal.t Map.M(String).t -> t
