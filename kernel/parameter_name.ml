open! Core0

type t = string [@@deriving bin_io, compare, sexp]

let equal = [%compare.equal: t]
let of_string n = n
let to_string n = n
