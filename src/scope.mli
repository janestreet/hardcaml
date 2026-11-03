(** Scopes control the process of hierarchical circuit generation.

    They track a circuit database of instantiated modules, and a scheme for managing the
    naming of signals within the design. *)

open! Core0

module Path : sig
  type t [@@deriving sexp_of]

  val default_path_seperator : string
  val to_string : ?sep:string -> t -> string
  val to_list : t -> string list
end

(** Specifies which kinds of properties, and which groups, are traced by a scope. *)
module Trace_properties = Property_manager.Trace_properties

type t [@@deriving sexp_of]

(** [create ?flatten_design ?name ()] creates a new scope. If [flatten_design] is [true],
    then all module instantions are inlined. *)
val create
  :  ?flatten_design:bool (** default [false] *)
  -> ?auto_label_hierarchical_ports:bool (** default [false] *)
  -> ?trace_properties:Trace_properties.t (** default [Trace_properties.none] *)
  -> unit
  -> t

(** [sub_scope t label] returns a new scope with (mangled) [label] appended to its
    hierarchical path. The returned scope shares property management with [t], so
    properties added in the sub-scope are emitted by [t]'s circuit config. *)
val sub_scope : t -> string -> t

(** Like [sub_scope t label], but also reinitializes circuit-specific state, specifically
    property management. Use this when creating a separate child circuit. *)
val hierarchy_sub_scope : t -> string -> t

(** [path t] returns the {!Path.t} associated with [t]. This will determine the prefix
    used when naming modules that are associated with this scope. *)
val path : t -> Path.t

(** [circuit_database t] returns the circuit database associated with [t]. Note that
    circuit databases are shared among {!sub_scope}s. *)
val circuit_database : t -> Circuit_database.t

(** [flatten_design t] returns true when HardCaml will inline all module instantiations. *)
val flatten_design : t -> bool

(** [auto_label_hierarical_ports t] returns true when Hardcaml will add names to input and
    outputs ports of each hierarchical module.

    This is useful with the interactive waveform viewer. The port names are prefixed with
    [i$] and [o$] and will be arranged in a tree view automactically. *)
val auto_label_hierarchical_ports : t -> bool

(** [trace_properties t] returns the property tracing configuration of [t]. *)
val trace_properties : t -> Trace_properties.t

(** [name ?sep t signal string] creates a hierarchical name based on the path of [t] and
    [string]. [sep], when provided, determines the separator for path components in the
    hierarchical name (default is [$]). *)
val name : ?sep:string -> t -> string -> string

(** Return the current (mangled) instance name. The top level module has no instance name,
    so returns [None]. *)
val instance : t -> string option

(** Creates a hierarchical name, built with [name], and applies it to the signal.

    This is typically used as a partial application to construct a new signal naming
    operator, .e.g:

    {[
      let (--) = naming scope in
      let named_signal = some_signal -- "data" in
    ]} *)
val naming
  :  ?sep:string
  -> t
  -> ?loc:Stdlib.Lexing.position
  -> Signal.t
  -> string
  -> Signal.t

(** Creates an atomic proposition for use in an LTL formula, naming the AP with the
    scope's name and the provided string argument *)
val make_ltl_ap : t -> string -> Signal.t -> Signal.t Property.LTL.Formula.t

val add_assertion : ?group:Property_manager.Group.t -> t -> string -> Signal.t -> unit

val add_ltl_property
  :  ?group:Property_manager.Group.t
  -> t
  -> string
  -> Property.LTL.t
  -> unit

val add_sva_property
  :  ?group:Property_manager.Group.t
  -> t
  -> string
  -> Signal.t Property.Sva.t
  -> unit

(** Returns the (live, unfiltered) property manager of [t], or [None] if no property type
    is enabled in its {!trace_properties}. Filtering by type and group happens where the
    properties are consumed (e.g. via {!circuit_config_with_properties}). *)
val property_manager : t -> Property_manager.t option

(** Return a circuit config which emits the scope's properties, as selected by its
    {!trace_properties}. *)
val circuit_config_with_properties
  :  ?circuit_config:Circuit.Config.t
  -> t
  -> Circuit.Config.t

(** The circuit config used when building a hierarchical circuit from [t]. Equivalent to
    {!circuit_config_with_properties} with no [circuit_config] override. *)
val circuit_config_for_hierarchy : t -> Circuit.Config.t
