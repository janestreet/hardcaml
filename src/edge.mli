type t =
  | Rising
  | Falling
[@@deriving sexp_of]

val to_int : t -> int
