(** Specification of rising or falling edge of a signal (normally a clock). *)

type t =
  | Rising
  | Falling
[@@deriving sexp_of, equal ~localize, compare ~localize]

val to_int : t -> int
