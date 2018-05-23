(** Linear feedback shift registers *)

open! Import

module Config : sig
  type t = Galois | Fibonacci
  [@@deriving enumerate, sexp_of]
end

module Op : sig
  type t = Xor | Xnor
  [@@deriving enumerate, sexp_of]
end

(** Create the update logic for a lfsr.  Used in conjuction with [reg_fb] to construct
    a complete [lfsr].

    - Shift register sizes can be between 2 and 168 bits.
    - [Galois] or [Fibonacci] forms are supported - prefer [Galois] in general as it has a
      shorter critical path.
    - The basic gate can be [xor] or [xnor].  With [xor] the all 0's state is invalid
      while with [xnor] the all 1's state is invalid.
    - The lfsrs generated are complete according to xapp052 - this means they will
      sequence through all possible states (except the invalid one) before repeating.
    - All complete lfsr have a counterpart organisation of the taps which leads to
      a second (but still complete) sequence. *)
val create
  :  ?config : Config.t (** default is [Galois]. *)
  -> ?counterpart_taps : bool (** default is [false]. *)
  -> ?op : Op.t (** default is [Xor] *)
  -> (module Hardcaml.Comb.S with type t = 'a)
  -> 'a
  -> 'a
