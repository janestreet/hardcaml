(** Specification of a signals level - high or low. *)

type t =
  | High
  | Low
[@@deriving bin_io, sexp_of, equal, compare]

val to_int : t -> int
