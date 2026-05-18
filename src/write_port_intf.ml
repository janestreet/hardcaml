(** A memory write port. *)

open! Core0

module type S = sig
  type 'a t =
    { write_clock : 'a
    ; write_address : 'a
    ; write_enable : 'a
    ; write_data : 'a
    }
  [@@deriving sexp_of, equal ~localize, compare ~localize]

  include Hardcaml_interface_types.Pre_partial with type 'a t := 'a t

  val port_names : string t
end

module type Write_port = sig
  module type S = S

  include S
  include Bin_prot.Binable.S1 with type 'a t := 'a t
end
