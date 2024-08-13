(** Specification of a signals level - high or low. *)

type t =
  | High
  | Low
[@@deriving sexp_of, equal, compare]

val to_int : t -> int
