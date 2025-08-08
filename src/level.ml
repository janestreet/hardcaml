type t =
  | High
  | Low
[@@deriving sexp_of, equal ~localize, compare ~localize]

let to_int = function
  | High -> 1
  | Low -> 0
;;
