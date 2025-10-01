open Ppxlib

type t =
  | Iter
  | Map

val name : t -> string
val name2 : t -> string
val option_map : t -> location -> expression
val option_map2_exn : t -> location -> expression
