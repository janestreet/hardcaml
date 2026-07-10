(** A memory read port. *)

open! Core0

module type S = sig
  type 'a t =
    { read_clock : 'a
    ; read_address : 'a
    ; read_enable : 'a
    }
  [@@deriving bin_io, sexp_of, equal ~localize, compare ~localize]

  include Hardcaml_interface_types.Pre_partial with type 'a t := 'a t

  val port_names : string t
end

module type Read_port = sig
  module type S = S

  include S
end
