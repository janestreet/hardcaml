open Base

module Slot : sig
  type t = Stdlib.Printexc.Slot.t [@@deriving sexp_of, compare, equal, hash]

  val format : t -> string
  val format_location : Stdlib.Printexc.location -> string
end

type t = Slot.t list [@@deriving sexp_of, compare, equal, hash]
