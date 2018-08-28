type t =
  | High
  | Low
[@@deriving sexp_of]

val to_int : t -> int
