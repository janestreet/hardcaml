type t =
  | Rising
  | Falling
[@@deriving bin_io, sexp_of, equal ~localize, compare ~localize]

let to_int = function
  | Rising -> 1
  | Falling -> 0
;;
