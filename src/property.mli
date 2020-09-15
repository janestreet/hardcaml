type atomic_proposition = Signal.t [@@deriving sexp_of]

module CTL : sig
  type state =
    | True
    | P of atomic_proposition
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
  val p : atomic_proposition -> state

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
  val to_string : ?name:(atomic_proposition -> string) -> state -> string

  (** Find all atomic propositions in the formula *)
  val atomic_propositions : state -> atomic_proposition list

  val map_atomic_propositions
    :  state
    -> f:(atomic_proposition -> atomic_proposition)
    -> state
end

module LTL : sig
  type path =
    | True
    | P of atomic_proposition
    | Pn of atomic_proposition
    | And of path * path
    | Or of path * path
    | Not of path
    | X of path
    | U of path * path
    | R of path * path
    | F of path
    | G of path
  [@@deriving sexp_of]

  (** True *)
  val vdd : path

  (** False *)
  val gnd : path

  (** Proposition *)
  val p : atomic_proposition -> path

  (** And *)
  val ( &: ) : path -> path -> path

  (** Or *)
  val ( |: ) : path -> path -> path

  (** Not *)
  val ( ~: ) : path -> path

  (** Xor *)
  val ( ^: ) : path -> path -> path

  (** Implication *)
  val ( ==>: ) : path -> path -> path

  (** Does not equal *)
  val ( <>: ) : path -> path -> path

  (** Equals *)
  val ( ==: ) : path -> path -> path

  (** In the next step *)
  val x : ?n:int -> path -> path

  (** In [u a b] [a] holds at least until [b] holds *)
  val u : path -> path -> path

  (** Release *)
  val r : path -> path -> path

  (** Finally(/eventually) *)
  val f : path -> path

  (** Globally/(always) *)
  val g : path -> path

  (** Weak until *)
  val w : path -> path -> path

  (** Convert property to a string *)
  val to_string : ?name:(atomic_proposition -> string) -> path -> string

  (** Find all atomic propositions in the formula *)
  val atomic_propositions : path -> atomic_proposition list

  (** Apply [f] to all atomic propositions in the formula *)
  val map_atomic_propositions
    :  path
    -> f:(atomic_proposition -> atomic_proposition)
    -> path

  (** Maximum nested depth of formula *)
  val depth : path -> int

  (** Convert to negative normal form. *)
  val nnf : path -> path

  (** Rewrite the formula only look a limited distance into the future. *)
  val limit_depth : int -> path -> path
end
