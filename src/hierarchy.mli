open! Import

module With_interface (I : Interface.S) (O : Interface.S) : sig
  (** [create database ~name create_fn inputs] creates a sub-circuit using [create_fn
      inputs] and adds it to [database].  It is then referenced in current circuit by an
      instantiation. *)
  val create
    : (Circuit_database.t
       -> name : string
       -> Circuit.With_interface(I)(O).create
       -> Circuit.With_interface(I)(O).create) Circuit.with_create_options
end
