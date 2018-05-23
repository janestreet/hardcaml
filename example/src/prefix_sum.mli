(** Parallel prefix networks.  See https://en.wikipedia.org/wiki/Prefix_sum.

    These require an associative operator, but do not depend on commutativity. *)

module Config : sig
  type t =
    | Serial      (** Simple serial prefix structure.  Used for testing *)
    | Sklansky    (** Sklansky's parallel prefix structure.  High fanout *)
    | Brent_kung  (** Brent-Kung parallel prefix structure.  Lower fanout, more hardware *)
    | Kogge_stone (** Kooge-Stone parallel prefix structure.  Large but fast *)
  [@@deriving enumerate, sexp_of]
end

val eval
  :  config : Config.t
  -> operator : ('a -> 'a -> 'a)
  -> 'a list
  -> 'a list

(** Parallel prefix adder. *)
val create
  :  (module Hardcaml.Comb.S with type t = 'a)
  -> config : Config.t
  -> input1 : 'a    (** width N *)
  -> input2 : 'a    (** width N *)
  -> carry_in : 'a  (** width 1 *)
  -> 'a
