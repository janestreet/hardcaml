open! Core0

type t =
  | Signed
  | Unsigned
[@@deriving sexp_of, equal ~localize]
