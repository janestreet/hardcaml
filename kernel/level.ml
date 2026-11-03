type t =
  | High
  | Low
[@@deriving bin_io, sexp_of, equal, compare]

let to_int = function
  | High -> 1
  | Low -> 0
;;
