(** RTL name of parameters on instantiated modules. *)

open! Core0

type t [@@deriving bin_io, compare ~localize, sexp]

include%template Equal.S [@mode local] with type t := t

include Stringable.S with type t := t
