(** Scopes control the process of hierarchical circuit generation.

    They track a circuit database of instantiated modules, and a scheme for managing the
    naming of signals within the design. *)

open Base

module Path : sig
  type t [@@deriving sexp_of]

  val default_path_seperator : string
  val to_string : ?sep:string -> t -> string
  val to_list : t -> string list
end

(** Control of name generation in a hierarchy of modules. The position of a module within
    a hierarchy is determined by a path which leads back to the (single) top most parent
    module. Signal names may be pre-pended with some representation of that path.

    - [No_path]    - Nothing is added to the name.
    - [Local_path] - Only the name of the enclosing module is added to the name.
    - [Full_path]  - The full path is included in the name
    - [Auto]       - The full path is tracked in the scope, but names have no path.

    Generally hierarchical names are taken from the circuit name, though it is possible to
    specify a different instantiation name. These names are mangled so they are unique
    within each scope.

    [Auto] mode works in conjunction with the [Hierarchy] module to automatically rewrite
    names with full paths without having to work with explicit path names.
*)
module Naming_scheme : sig
  type t =
    | Auto
    | Full_path
    | Local_path
    | No_path
  [@@deriving equal, sexp_of]
end

type t [@@deriving sexp_of]

(** [create ?flatten_design ?naming_scheme ?name ()] creates a new scope. If
    [flatten_design] is [true], then all module instantions are inlined. Names for wires
    are determined by [naming_scheme]. *)
val create
  :  ?flatten_design:bool (** default [false] *)
  -> ?auto_label_hierarchical_ports:bool (** default [false] *)
  -> ?trace_properties:bool (** default [false] *)
  -> ?naming_scheme:Naming_scheme.t
       (** defaults to [Full_path] when [flatten_design] is [true] and [No_path] otherwise. *)
  -> ?name:string
  -> unit
  -> t

(** [sub_scope t label] returns a new scope with (mangled) [label] appended to its
    hierarchical path *)
val sub_scope : t -> string -> t

(** [path t] returns the {!Path.t} associated with [t]. This will determine the
    prefix used when naming modules that are associated with this scope. *)
val path : t -> Path.t

(** [circuit_database t] returns the circuit database associated with [t]. Note
    that circuit databases are shared among {!sub_scope}s. *)
val circuit_database : t -> Circuit_database.t

(** [flatten_design t] returns true when HardCaml will inline all module
    instantiations. *)
val flatten_design : t -> bool

(** [auto_label_hierarical_ports t] returns true when Hardcaml will add names to input and
    outputs ports of each hierarchical module.

    This is useful with the interactive waveform viewer. The port names are prefixed with
    [i$] and [o$] and will be arranged in a tree view automactically. *)
val auto_label_hierarchical_ports : t -> bool

(** [trace_properties t] returns true when tracing of ltl properties is enabled *)
val trace_properties : t -> bool

(** [naming_scheme t] returns the {!Naming.t} that [t] was constructed with. *)
val naming_scheme : t -> Naming_scheme.t

(** [name ?sep t signal string] creates a heirarchical name based on the path of [t] and
    [string]. [sep], when provided, determines the separator for path components in the
    heirarchical name (default is [$]). *)
val name : ?sep:string -> t -> string -> string

(** Return the current (mangled) instance name. The top level module has no instance name,
    so returns [None]. *)
val instance : t -> string option

(** Creates a hierarchical name, built with [name], and applies it to the signal.

    This is typically used as a partial application to construct a new signal
    naming operator, .e.g:

    {[
      let (--) = naming scope in
      let named_signal = some_signal -- "data" in
    ]} *)
val naming : ?sep:string -> t -> Signal.t -> string -> Signal.t

(** Creates an atomic proposition for use in an LTL formula,
    naming the AP with the scope's name and the provided string argument *)
val make_ltl_ap : t -> string -> Signal.t -> Property.LTL.path

val add_ltl_property : t -> string -> Property.LTL.path -> unit

(* [assertion_manager t] returns the {!Assertion_manager.t} associated with [t]. Note
   that assertion managers are shared among {!sub_scope}s. *)
val assertion_manager : t -> Assertion_manager.t option
val property_manager : t -> Property_manager.t option
