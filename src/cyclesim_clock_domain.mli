open! Core0

module Name : sig
  type t [@@deriving bin_io, compare ~localize, sexp, hash]

  include Stringable.S with type t := t
  include Comparable.S_plain with type t := t
end

type t =
  { name : Name.t
  ; period : int (** Cycles in a clocks period. *)
  }
[@@deriving sexp_of]

val create : name:string -> period:int -> t
val create_list : (string * int) list -> t list

(** Whether or not the cycle is aligned with the clock's period. *)
val aligned : t -> cycle:int -> bool

(** Utilities for operating of a set of clock domains. Each element in the group is
    assigned a unique index out of a dense set. These indices can then be used to create
    array backed maps and sets. *)

type domain = t
type indexed [@@deriving sexp_of]

val domain : indexed -> domain

module Group : sig
  type t [@@deriving sexp_of]

  val create : domain list -> [ `Ok of t | `Duplicate_key of Name.t ]
  val create_exn : domain list -> t
  val get : t -> Name.t -> indexed option
  val elements : t -> indexed iarray
end

module Table : sig
  type 'a t [@@deriving sexp_of]

  val create : Group.t -> 'a -> 'a t
  val init : Group.t -> f:(indexed -> 'a) -> 'a t
  val set : 'a t -> key:indexed -> data:'a -> unit
  val get : 'a t -> indexed -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  val map_inplace : 'a t -> f:('a -> 'a) -> unit
end

module Set : sig
  type t [@@deriving sexp_of, to_string]

  val create : Group.t -> default:bool -> t
  val clear : t -> unit
  val add : t -> indexed -> unit
  val remove : t -> indexed -> unit
  val mem : t -> indexed -> bool
  val iter : t -> f:(indexed -> unit) -> unit
end
