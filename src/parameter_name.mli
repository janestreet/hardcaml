(** RTL name of parameters on instantiated modules. *)


open! Import

type t [@@deriving compare, sexp]

include Equal.S with type t := t
include Stringable.S with type t := t
