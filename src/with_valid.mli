(** Uses a [valid] bit to indicate the validity of a [value].  Conceptually similar to an
    [Option.t]. *)

open! Import

type 'a t =
  { valid : 'a
  ; value : 'a }
[@@deriving sexp_of]

val map : 'a t -> f:('a -> 'b) -> 'b t
