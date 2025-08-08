(** RTL name of parameters on instantiated modules. *)

open Base

type t [@@deriving compare ~localize, sexp]

include%template Equal.S [@mode local] with type t := t

include Stringable.S with type t := t
