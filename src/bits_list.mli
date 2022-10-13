(** Combinational logic described as lists of [Bit]s ie [0;1;1;1;0].

    Width implicitly defined as the length of the list. *)

open Base

module type T = sig
  type t [@@deriving equal, sexp]

  (** Only constants are valid (ie the type [t] does not represent a notion of inputs).
      This allows more compact sexp printing. *)
  val constant_only : bool

  (** Mux's should be optimised. The select values will be converted to an int and used
      access the data list. Only valid in conjunction with [constant_only]. *)
  val optimise_muxs : bool

  val vdd : t
  val gnd : t
  val ( &: ) : t -> t -> t
  val ( |: ) : t -> t -> t
  val ( ^: ) : t -> t -> t
  val ( ~: ) : t -> t
  val to_char : t -> char
  val of_char : char -> t
end

module type Comb = sig
  include Comb.S

  val t_of_sexp : Sexp.t -> t
end

(** From [T] construct the combinational API *)
module Make (T : T) : Comb with type t = T.t list

(** Uses [0,1] as the bit type. *)
module Int_comb : Comb with type t = int list

(** Uses [false,true] as the bit type. *)
module Bool_comb : Comb with type t = bool list

(** Logic with undefined values. *)
module X : sig
  type t =
    | F
    | T
    | X
  [@@deriving compare, equal, sexp_of]

  include T with type t := t
end

(** Uses [F, T, X] as the bit type. *)
module X_comb : Comb with type t = X.t list
