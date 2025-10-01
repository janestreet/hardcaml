(** Some convenience functions for dealing with the iteration functions over different
    collections *)
open Ppxlib

type t =
  | List
  | Array
  | Iarray

val to_string : t -> string
val of_string : label -> t
val is_supported : label -> bool
val to_module_expr : t -> location -> expression
val map : t -> iter_or_map:Iter_or_map.t -> location -> expression
val mapi : t -> iter_or_map:Iter_or_map.t -> location -> expression
val map2 : t -> iter_or_map:Iter_or_map.t -> location -> expression
val init : t -> location -> expression
val to_list : t -> location -> expression
