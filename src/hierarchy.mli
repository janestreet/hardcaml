(** Allow a hardcaml circuit to be defined as a hierarchy of modules, rather than just a
    single flat module. *)

open! Core0

(** Fold through every circuit and instantiation in a hierarchical design.

    [f] will be passed the corresponding circuit if it exists in the database, and a
    [Signal.instanation] specification, unless the top most module. *)
val fold
  :  Circuit.t
  -> Circuit_database.t
  -> init:'a
  -> f:('a -> Circuit.t option -> Signal.t Signal.Type.Inst.Instantiation.t option -> 'a)
  -> 'a

val print : Circuit.t -> Circuit_database.t -> unit

module How_to_instantiate : sig
  type t =
    | Inlined
    | Inlined_in_scope
    | Hierarchical
    | Hierarchical_or_inlined_by_scope
  [@@deriving sexp_of]
end

(** Support for hierarchically structured Hardcaml designs. We extend the standard
    [Interface.Create_fn] pattern so that the create function also takes a [Scope.t]
    argument. This allows scoping of signal names and automatic recording of the design in
    a [Circuit_database.t].

    The scope argument controls construction of a flat or modular design as required for
    simulation or syntheis. *)
module In_scope (I : Interface.S) (O : Interface.S) : sig
  type create_fn = Scope.t -> Interface.Create_fn(I)(O).t

  (** Create a Hardcaml child design hierarchically, depending on [how_to_instantiate].

      - [Inlined] just calls the given [create_fn] and inlines the circuit.
      - [Inlined_in_scope] also inlines, but ensures naming is hierarchical
      - [Hierarchical] inserts an instantiation into the parent design and adds the
        circuit implementation into a [Circuit_database.t].
      - [Hierarchical_or_inlined_by_scope] will use the mode [Inline_in_scope] if
        [flatten_design] is true, or [Hierarchical] otherwise. This is the default mode.

      [input_attributes] and [output_attributes] are applied to ports if a hierarchical
      circuit is created and not if the circuit is inlined. [attributes] are applied to
      the instantiation itself.

      If [name] is provided, it will be speficy the name of the module(/entity). If [here]
      is provided the module name is automatically derived from the calling file name. If
      used multiple times in the same file, each module will have the same name. However,
      hardcaml will mangle those names to produce a legal circuit.

      Note - this is based an internal Jane Street compiler feature which automatically
      provides the [here] argument. When used with the public compiler [here] or [name]
      must be explicitly provided.

      The [instance] parameter can be used to specify the instantiation and scope name, if
      provided. Otherwise [name] (or [here]) is used as the scope name, and the
      instantiation name is derived automatically. [name]s are mangled so they form unique
      hierarchical paths to each instantiatiated design. *)
  val hierarchical
    :  ?config:Circuit.Config.t
    -> ?instance:string
    -> ?attributes:Rtl_attribute.t list
    -> ?input_attributes:Rtl_attribute.t list I.t
    -> ?output_attributes:Rtl_attribute.t list O.t
    -> ?how_to_instantiate:How_to_instantiate.t
    -> ?name:string
    -> here:[%call_pos]
    -> scope:Scope.t
    -> create_fn
    -> Interface.Create_fn(I)(O).t
end
