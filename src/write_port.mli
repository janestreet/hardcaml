(** A memory write port. *)

open! Base

type 'a t =
  { write_clock : 'a
  ; write_address : 'a
  ; write_enable : 'a
  ; write_data : 'a
  }
[@@deriving sexp_of]

(** {2 A partial [Interface] implementation}

    Use with [Interface.Update] to create a full interface specification.
*)

val iter : 'a t -> f:('a -> unit) -> unit
val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val to_list : 'a t -> 'a list
val port_names : string t
