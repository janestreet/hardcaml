(** A database which holds a collection of circuits, indexed by a unique circuit name.

    The database is used to map occurrences of instantiations within a Hardcaml circuit to
    an implementation.  This can then be used to generate an RTL module hierarchy.
    Structurally identical circuits may be indentified and the implementation shared. *)

type t [@@deriving sexp_of]

(** Create an empty database. *)
val create : unit -> t

(** Insert a circuit into the database and return a potentially modified circuit name,
    which should used when creating a Hardcaml instantiation.  If [share] is true and the
    database contains a structurally equal circuit then a reference to the existing
    circuit is returned.  Otherwise, a new entry is created. *)
val insert : ?share:bool (** default is [true] **) -> t -> Circuit.t -> string

(** Find a circuit in the database, given its name. *)
val find : t -> mangled_name:string -> Circuit.t option

(** Return a list of all circuits in the database. *)
val get_circuits : t -> Circuit.t list
