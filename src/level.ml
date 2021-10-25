type t =
  | High
  | Low
[@@deriving sexp_of, equal]

let to_int = function
  | High -> 1
  | Low -> 0
;;
