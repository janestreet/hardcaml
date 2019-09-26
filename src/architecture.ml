open Import

type t =
  | Small
  | Balanced
  | Fast
[@@deriving sexp]

let of_string = function
  | "small" -> Small
  | "balanced" -> Balanced
  | "fast" -> Fast
  | arch -> raise_s [%message "Unknow architecture selected" (arch : string)]
;;

let default = Balanced
