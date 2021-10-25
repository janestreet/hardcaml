type t =
  | Rising
  | Falling
[@@deriving sexp_of, equal]

let to_int = function
  | Rising -> 1
  | Falling -> 0
;;
