open! Core0

type t = string [@@deriving bin_io, compare ~localize, sexp]

let%template equal = ([%compare.equal: t] [@mode local]) [@@mode __ = (global, local)]
let of_string n = n
let to_string n = n
