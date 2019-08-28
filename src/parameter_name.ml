open! Import

type t = string [@@deriving compare, sexp]

let equal = [%compare.equal: t]
let of_string n = n
let to_string n = n
