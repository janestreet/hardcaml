type t =
  | High
  | Low
[@@deriving bin_io, sexp_of, equal ~localize, compare ~localize]

let to_int = function
  | High -> 1
  | Low -> 0
;;
