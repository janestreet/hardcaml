(** Map a set of names to a set of unique names. *)

open! Import

(** A mangler is a mapping from strings to the next available integer which should be
    added to the name to make it unique. *)
type t [@@deriving sexp_of]

(** Create a new mangler. *)
val create : case_sensitive:bool -> t

val add_identifier : t -> string -> [ `Ok | `Duplicate ]

(** Add a list of identifiers to the mangler table.  Raises if an identifier is already in
    the table. *)
val add_identifiers_exn : t -> string list -> unit

(** Test if the string is in the mangler, and return its mangler index if it is. *)
val find_index : t -> string -> int option

(** [mangle t name] returns a unique, mangled name. *)
val mangle : t -> string -> string
