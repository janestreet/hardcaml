open! Core

type t = Types.t [@@deriving sexp_of]

val from_file : string -> t
val from_string : string -> t
