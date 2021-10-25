(** Specification of a signals level - high or low. *)

type t =
  | High
  | Low
[@@deriving sexp_of, equal]

val to_int : t -> int
