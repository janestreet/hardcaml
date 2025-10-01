open! Base

type t [@@deriving bin_io, sexp_of]

val of_outputs : Signal.t list -> t
val to_outputs : t -> Signal.t list
