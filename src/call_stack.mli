open! Core0

module Slot : sig
  type t' =
    { filename : string
    ; line_number : int
    ; start_char : int
    ; defname : string
    }

  type t = t' option [@@deriving bin_io, sexp_of, compare, equal, hash]

  val create : Stdlib.Printexc.Slot.t -> t
  val format : t -> string
end

type t = Slot.t list [@@deriving bin_io, sexp_of, compare, equal, hash]
