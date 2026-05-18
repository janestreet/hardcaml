open! Core0

type t [@@deriving sexp_of, equal ~localize]

val from_raw_external_name : string -> t
val backend_agnostic : t -> string

module For_backend : sig
  type name = t
  type t [@@deriving sexp_of, to_string, equal ~localize]

  val backend_agnostic : t -> name
  val backend_agnostic_string : t -> string

  include Comparable.S_plain with type t := t
end

val legalize : t -> language:Rtl_language.t -> For_backend.t

module Scope : sig
  type name := t
  type t [@@deriving sexp_of]

  val create : unit -> t
  val add_port_name : t -> Signal.t -> string -> name
  val add_phantom_port_name : t -> string -> name
  val mangle_name : t -> string -> name
  val mangle_signal_names : t -> Signal.t -> name list
  val mangle_instantiation_name : t -> Signal.t -> name

  (** Returns the mangled name for the memory array, and (in VHDL) array type. *)
  val mangle_multiport_mem_name : t -> Signal.t -> name * name
end

module For_test : sig
  val of_string : string -> t
end
