(** Allow a hardcaml circuit to be defined as a hierarchy of modules, rather than just a
    single flat module. *)

open Base

val hierarchical
  :  (module Interface.S_Of_signal with type Of_signal.t = 'i)
  -> (module Interface.S_Of_signal with type Of_signal.t = 'o)
  -> ?config:Circuit.Config.t
  -> ?instance:string
  -> ?attributes:Rtl_attribute.t list
  -> scope:Scope.t
  -> name:string
  -> (Scope.t -> 'i -> 'o)
  -> 'i
  -> 'o

(** Fold through every circuit and instantiation in a hierarchical design.

    [f] will be passed the corresponding circuit if it exists in the database, and a
    [Signal.instanation] specification, unless the top most module.
*)
val fold
  :  Circuit.t
  -> Circuit_database.t
  -> init:'a
  -> f:('a -> Circuit.t option -> Signal.Type.instantiation option -> 'a)
  -> 'a

val print : Circuit.t -> Circuit_database.t -> unit

module With_interface (I : Interface.S) (O : Interface.S) : sig
  (** [create database ~name create_fn inputs] creates a sub-circuit using [create_fn
      inputs] and adds it to [database].  It is then referenced in current circuit by an
      instantiation. *)
  val create
    :  ?attributes:Rtl_attribute.t list
    -> ?config:Circuit.Config.t
    -> ?instance:string
    -> Circuit_database.t
    -> name:string
    -> Circuit.With_interface(I)(O).create
    -> Circuit.With_interface(I)(O).create
end

(** Support for hierarchically structured Hardcaml designs. We extend the standard
    [Interface.Create_fn] pattern so that the create function also takes a [Scope.t]
    argument. This allows scoping of signal names and automatic recording of the design
    in a [Circuit_database.t].

    The scope argument controls construction of a flat or modular design as required for
    simulation or syntheis. *)
module In_scope (I : Interface.S) (O : Interface.S) : sig
  type create = Scope.t -> Interface.Create_fn(I)(O).t

  (** Create a Hardcaml child design and link it into the parent design.  This will not
      form a hierarchical structure, but the signal naming will still be scoped. *)
  val create
    :  scope:Scope.t
    -> name:string
    -> create
    -> Circuit.With_interface(I)(O).create

  (** Create a Hardcaml child design hierarchically, depending on the construction mode.
      In a flat design [In_scope.create] is called.  In a hierarchical design a circuit is
      constructed, added to a [Circuit_database.t] and an instantiation inserted into the
      parent design.

      The [instance] parameter can be used to specify the instantiation and scope name, if
      provided. Otherwise [name] is used as the scope name, and the instantiation name is
      derived automatically. [name]s are mangled so they form unique hierarchical paths to
      each instantiatiated design. *)
  val hierarchical
    :  ?config:Circuit.Config.t
    -> ?instance:string
    -> ?attributes:Rtl_attribute.t list
    -> scope:Scope.t
    -> name:string
    -> create
    -> Circuit.With_interface(I)(O).create
end
