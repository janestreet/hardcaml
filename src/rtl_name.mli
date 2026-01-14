open! Core0

type t

val create : Rtl_language.t -> t
val add_port_name : t -> Signal.t -> string -> unit
val add_phantom_port_name : t -> string -> unit
val mangle_name : t -> string -> string
val mangle_signal_names : t -> Signal.t -> string list
val mangle_instantiation_name : t -> Signal.t -> string

(** Returns the mangled name for the memory array, and (in VHDL) array type. *)
val mangle_multiport_mem_name : t -> Signal.t -> string * string
