open Core0

module Directive : sig
  type t =
    | Assert
    | Assume
    | Cover
  [@@deriving sexp_of, equal, enumerate]

  module Unstable : sig
    type nonrec t = t [@@deriving bin_io, sexp]
  end

  val to_string : t -> string
end

module Clocked : sig
  include module type of Clocking.Property_clocked
end

type ('var, 'formula) property =
  { directive : Directive.t
  ; clocked : 'var Clocked.t option
  ; formula : 'formula
  }
[@@deriving sexp_of]

(** CTL formula. Can be used with [Hardcaml_formal_verification.Nusmv] for symbolic model
    checking. *)
module CTL : sig
  module Formula : sig
    type state =
      | True
      | P of Signal.t
      | And of state * state
      | Not of state
      | E of path
      | A of path

    and path =
      | X of state
      | U of state * state
      | F of state
      | G of state
    [@@deriving sexp_of]

    (** True *)
    val t : state

    (** Proposition *)
    val p : Signal.t -> state

    (** And *)
    val ( &: ) : state -> state -> state

    (** Not *)
    val ( ~: ) : state -> state

    (** On all paths in the next state *)
    val ax : ?n:int -> state -> state

    (** On some path in the next state *)
    val ex : ?n:int -> state -> state

    (** On all paths [u] holds. *)
    val au : state -> state -> state

    (** On some path [u] holds *)
    val eu : state -> state -> state

    (** On all paths finally holds *)
    val af : state -> state

    (** On some path finally holds *)
    val ef : state -> state

    (** On all paths globally holds *)
    val ag : state -> state

    (** On some path globally holds *)
    val eg : state -> state

    (** Convert property to a string *)
    val to_string : proposition:(Signal.t -> string) -> state -> string

    (** Find all atomic propositions in the formula *)
    val atomic_propositions : state -> Signal.t list

    val map_atomic_propositions : state -> f:(Signal.t -> Signal.t) -> state
  end
end

(** LTL formula. Can be used with [Hardcaml_formal_verification.Nusmv] for symbolic model
    checking and [Hardcaml_runtime_verification_cyclesim] for runtime simulation monitors. *)
module LTL : sig
  module Formula : sig
    type 'var t =
      | True
      | P of 'var
      | Pn of 'var
      | And of 'var t * 'var t
      | Or of 'var t * 'var t
      | Not of 'var t
      | X of 'var t
      | U of 'var t * 'var t
      | R of 'var t * 'var t
      | F of 'var t
      | G of 'var t
    [@@deriving sexp_of]

    module Unstable : sig
      type nonrec 'var t = 'var t [@@deriving bin_io, sexp]
    end

    (** True *)
    val vdd : _ t

    (** False *)
    val gnd : _ t

    (** Proposition over a hardware signal. Raises if the signal is not 1 bit wide. *)
    val p : Signal.t -> Signal.t t

    (** Proposition over an arbitrary atom type. No width checking is (or can be)
        performed; when the formula is ultimately evaluated against hardware or waveform
        data the atom must correspond to a 1 bit value. *)
    val atom : 'var -> 'var t

    (** And *)
    val ( &: ) : 'var t -> 'var t -> 'var t

    (** Or *)
    val ( |: ) : 'var t -> 'var t -> 'var t

    (** Not *)
    val ( ~: ) : 'var t -> 'var t

    (** Xor *)
    val ( ^: ) : 'var t -> 'var t -> 'var t

    (** Implication *)
    val ( ==>: ) : 'var t -> 'var t -> 'var t

    (** Does not equal *)
    val ( <>: ) : 'var t -> 'var t -> 'var t

    (** Equals *)
    val ( ==: ) : 'var t -> 'var t -> 'var t

    (** In the next step *)
    val x : ?n:int -> 'var t -> 'var t

    (** In [u a b] [a] holds at least until [b] holds *)
    val u : 'var t -> 'var t -> 'var t

    (** Release *)
    val r : 'var t -> 'var t -> 'var t

    (** Finally(/eventually) *)
    val f : 'var t -> 'var t

    (** Globally/(always) *)
    val g : 'var t -> 'var t

    (** Weak until *)
    val w : 'var t -> 'var t -> 'var t

    (** Convert property to a string *)
    val to_string : proposition:('var -> string) -> 'var t -> string

    (** Find all atomic propositions in the formula *)
    val atomic_propositions : 'var t -> 'var list

    (** Apply [f] to all atomic propositions in the formula *)
    val map_atomic_propositions : 'a t -> f:('a -> 'b) -> 'b t

    (** Maximum nested depth of formula *)
    val depth : _ t -> int

    (** Convert to negative normal form. *)
    val nnf : 'var t -> 'var t

    (** Rewrite the formula only look a limited distance into the future. *)
    val limit_depth : int -> 'var t -> 'var t
  end

  type t = (Signal.t, Signal.t Formula.t) property [@@deriving sexp_of]

  val asserts : ?clocked:Signal.t Clocked.t -> Signal.t Formula.t -> t
  val assume : ?clocked:Signal.t Clocked.t -> Signal.t Formula.t -> t
  val cover : ?clocked:Signal.t Clocked.t -> Signal.t Formula.t -> t
  val create : ?clocked:Signal.t Clocked.t -> Directive.t -> Signal.t Formula.t -> t
end

(** System verilog assertions. Converted to (System) Verilog properties, and interpreted
    by simulation or formal verification backends.

    SVA properties can be converted from LTL properties. *)
module Sva : sig
  module Formula : sig
    module Count : sig
      type t = int [@@deriving sexp_of]
    end

    module Range : sig
      type t =
        { from : int
        ; to_ : int
        }
      [@@deriving sexp_of]
    end

    module Sequence_range : sig
      type t = private
        | Bounded of Range.t
        | At_least of int
        | At_most of int
      [@@deriving sexp_of]

      val bounded_exn : Range.t -> t
      val at_least_exn : int -> t
      val at_most_exn : int -> t
    end

    type sequence =
      [ `Sequence
      | `Property
      ]
    [@@deriving sexp_of]

    type property = [ `Property ] [@@deriving sexp_of]

    (** Kind_witness.t is used to denote whether the AST node can be used to represent
        only a Sequence or a Property [Sequence] case or just a property [Property] *)
    module Kind_witness : sig
      type _ t =
        | Sequence : sequence t
        | Property : property t
    end

    type ('var, 'kind) t =
      (* Sequence layer: a boolean is a sequence, and a sequence is usable as a property
         too, so leave 'kind free (ordinary constructors). *)
      | True : ('var, sequence) t
      | Atom : 'var -> ('var, sequence) t
      | Delay :
          (('var, sequence) t option * Sequence_range.t * ('var, sequence) t)
          -> ('var, sequence) t
      | Repeat : (('var, sequence) t * Sequence_range.t) -> ('var, sequence) t
      | Throughout : ('var * ('var, sequence) t) -> ('var, sequence) t
      | Overlapping_implication :
          ('var, sequence) t * ('var, property) t
          -> ('var, property) t
      (* connectives on both properties and sequences *)
      | And : ('var, 'kind) t * ('var, 'kind) t -> ('var, 'kind) t
      | Or : ('var, 'kind) t * ('var, 'kind) t -> ('var, 'kind) t
      (* Property-only connectives. *)
      | Not : ('var, [> property ]) t -> ('var, property) t
      | Property_implication :
          ('var, [> property ]) t * ('var, [> property ]) t
          -> ('var, property) t
      | Iff : ('var, [> property ]) t * ('var, [> property ]) t -> ('var, property) t
      (* Property-only temporal operators. *)
      | Nexttime : ('var, [> property ]) t * Count.t option -> ('var, property) t
      | S_nexttime : ('var, [> property ]) t * Count.t option -> ('var, property) t
      | Always : ('var, [> property ]) t * Range.t option -> ('var, property) t
      | S_always : ('var, [> property ]) t * Range.t -> ('var, property) t
      | Eventually : ('var, [> property ]) t * Range.t -> ('var, property) t
      | S_eventually : ('var, [> property ]) t * Range.t option -> ('var, property) t
      | Until : ('var, [> property ]) t * ('var, [> property ]) t -> ('var, property) t
      | Until_with :
          ('var, [> property ]) t * ('var, [> property ]) t
          -> ('var, property) t
      | S_until : ('var, [> property ]) t * ('var, [> property ]) t -> ('var, property) t
      | S_until_with :
          ('var, [> property ]) t * ('var, [> property ]) t
          -> ('var, property) t
      | Sequence_as_property : ('var, sequence) t -> ('var, property) t

    val kind : ('var, 'kind) t -> 'kind Kind_witness.t
    val as_property : ('var, [> property ]) t -> ('var, property) t

    (** Convert a LTL property to SVA *)
    val of_ltl : 'var LTL.Formula.t -> ('var, property) t

    (** Convert SVA formula to a rope. For use in printing RTL *)
    val to_rope : proposition:('a -> Rope.t) -> ('a, 'kind) t -> Rope.t

    (** Return a list of atomic propositions referenced in this property *)
    val atomic_propositions : ('var, 'kind) t -> 'var list

    val map_atomic_propositions : ('a, 'kind) t -> f:('a -> 'b) -> ('b, 'kind) t
  end

  type 'var t = ('var, ('var, Formula.property) Formula.t) property [@@deriving sexp_of]

  val asserts
    :  ?clocked:'var Clocked.t
    -> ('var, [> Formula.property ]) Formula.t
    -> 'var t

  val assume
    :  ?clocked:'var Clocked.t
    -> ('var, [> Formula.property ]) Formula.t
    -> 'var t

  val cover : ?clocked:'var Clocked.t -> ('var, [> Formula.property ]) Formula.t -> 'var t

  val create
    :  ?clocked:'var Clocked.t
    -> Directive.t
    -> ('var, [> Formula.property ]) Formula.t
    -> 'var t

  (** {2 Internal API} *)

  val map_atomic_propositions : 'a t -> f:('a -> 'b) -> 'b t

  (** Return a list of atomic propositions referenced in this property, including the
      clock is any. *)
  val atomic_propositions : 'var t -> 'var list

  (** Convert SVA to a rope. For use in printing RTL *)
  val to_rope : proposition:('a -> Rope.t) -> 'a t -> Rope.t

  val of_ltl : LTL.t -> Signal.t t
end
