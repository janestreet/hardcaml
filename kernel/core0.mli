(* Destructive substitution effectively removes the [Signal] module from the module type *)
include module type of Core with module Signal := Core.Signal
