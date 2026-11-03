(** Specification of rising or falling edge of a signal (normally a clock). *)

type t =
  | Rising
  | Falling
[@@deriving bin_io, sexp_of, equal, compare]

val to_int : t -> int
