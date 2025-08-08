open Base

type t = string [@@deriving compare ~localize, sexp]

let%template equal = [%compare_local.equal: t] [@@mode __ = (global, local)]
let of_string n = n
let to_string n = n
