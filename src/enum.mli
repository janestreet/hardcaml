(** The [Enum] contains functors that can be used to create special interface
    modules to represent an enumeration type (ie: a variant with no arguments).
*)

include Enum_intf.Enum (** @inline *)
