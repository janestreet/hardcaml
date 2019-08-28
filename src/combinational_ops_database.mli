(** A database which holds a collecton of custom combinational operations for use with
    [Cyclesim] based simulators. *)

open! Import

type t [@@deriving sexp_of]

(** Create an empty database. *)
val create : unit -> t

(** Insert combinational op into the database. *)
val insert : t -> Combinational_op.t -> unit

(** Find a combinational op in the database, given its name. *)
val find : t -> name:string -> Combinational_op.t option

val fold : t -> init:'a -> f:('a -> Combinational_op.t -> 'a) -> 'a
val iter : t -> f:(Combinational_op.t -> unit) -> unit

(** [concat ts] creates a new database with all elements added from the list of databases
    [ts]. *)
val concat : t list -> t
