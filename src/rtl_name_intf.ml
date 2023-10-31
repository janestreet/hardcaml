open Base

module type Language = sig
  val is_valid_first_char : char -> bool
  val is_valid_other_char : char -> bool
  val replace_with : char
  val prefix : string
  val case_sensitive : bool
  val reserved_words : string list
end

module type Rtl_name = sig
  module type Language = Language

  module Verilog : Language
  module Vhdl : Language

  type t

  val create : (module Language) -> t
  val legalize : t -> string -> string
  val add_port_name : t -> Signal.t -> string -> unit
  val add_phantom_port_name : t -> string -> unit
  val mangle_name : t -> string -> string
  val mangle_signal_names : t -> Signal.t -> string list
  val mangle_instantiation_name : t -> Signal.t -> string

  (** Returns the mangled name for the memory array, and (in VHDL) array type. *)
  val mangle_multiport_mem_name : t -> Signal.t -> string * string
end
