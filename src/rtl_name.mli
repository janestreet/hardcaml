open! Core0

type t [@@deriving sexp_of]

val legalize : t -> language:Rtl_language.t -> string
val legalize_bare_name : string -> language:Rtl_language.t -> string

module Scope : sig
  type name := t
  type t

  val create : unit -> t
  val add_port_name : t -> Signal.t -> string -> name
  val add_phantom_port_name : t -> string -> name
  val mangle_name : t -> string -> name
  val mangle_signal_names : t -> Signal.t -> name list
  val mangle_instantiation_name : t -> Signal.t -> name

  (** Returns the mangled name for the memory array, and (in VHDL) array type. *)
  val mangle_multiport_mem_name : t -> Signal.t -> name * name
end
