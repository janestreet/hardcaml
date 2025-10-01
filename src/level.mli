(** Specification of a signals level - high or low. *)

type t =
  | High
  | Low
[@@deriving sexp_of, equal ~localize, compare ~localize]

val to_int : t -> int
