(** Scopes control the process of hierarchical circuit generation.

    They track a circuit database of instantiated modules, and a scheme for managing the
    naming of signals within the design. *)

open! Import

module Path : sig
  type t [@@deriving sexp_of]

  val to_string : ?sep:string (** defaults to ["$"] *) -> t -> string
  val to_list : t -> string list
end

(** Control of name generation in a hierarchy of modules. The position of a module within
    a hierarchy is determined by a path which leads back to the (single) top most parent
    module. Signal names may be pre-pended with some represtation of that path.

    - [No_path]    - Nothing is added to the name.
    - [Local_path] - Only the name of the enclosing module is added to the name.
    - [Full_path]  - The full path is included in the name *)
module Naming_scheme : sig
  type t =
    | Full_path
    | Local_path
    | No_path
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

(** [create ?flatten_design ?naming_scheme ?name ()] creates a new scope. If
    [flatten_design] is [true], then all module instantions are inlined. Names
    for wires are determiend by [naming_scheme]. *)
val create
  :  ?flatten_design:bool (** default [false] *)
  -> ?naming_scheme:Naming_scheme.t
  (** default [Full_path] when
      [flatten_design] is [true] and
      [No_path] otherwise. *)
  -> ?name:string
  -> unit
  -> t

(** [sub_scope t label] returns a new scope with [label] appended to its
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

(** [naming_scheme t] returns the {!Naming.t} that [t] was constructed with. *)
val naming_scheme : t -> Naming_scheme.t

(** [name ?sep t signal string] creates a heirarchical name based on the path of [t] and
    [string]. [sep], when provided, determines the separator for path components in the
    heirarchical name (default is [$]). *)
val name : ?sep:string -> t -> string -> string

(** Creates a hierarchical name, built with [name], and applies it to the signal.

    This is typically used as a partial application to construct a new signal
    naming operator, .e.g:

    {[
      let (--) = naming scope in
      (* ... other code ... *)
      let named_signal = some_signal -- "data" in
      (* ... more code ... *)
    ]} *)
val naming : ?sep:string -> t -> Signal.t -> string -> Signal.t
