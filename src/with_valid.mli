(** Uses a [valid] bit to indicate the validity of a [value].  Conceptually similar to an
    [Option.t]. *)

open! Import

type ('valid, 'value) t2 =
  { valid : 'valid
  ; value : 'value
  }
[@@deriving sexp_of]

type 'a t = ('a, 'a) t2 [@@deriving sexp_of]

val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val iter : 'a t -> f:('a -> unit) -> unit
val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val to_list : 'a t -> 'a list
