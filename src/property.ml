open! Core0

module Directive = struct
  module Unstable = struct
    type t =
      | Assert
      | Assume
      | Cover
    [@@deriving sexp, equal, enumerate, bin_io]
  end

  include Unstable

  let to_string = function
    | Assert -> "assert"
    | Assume -> "assume"
    | Cover -> "cover"
  ;;
end

module Clocked = struct
  include Clocking.Property_clocked

  let map { edge; clock; disable_iff } ~f =
    create ?disable_iff:(Option.map disable_iff ~f) edge (f clock)
  ;;

  let to_rope ~proposition { edge; clock; disable_iff } =
    let edge =
      match edge with
      | Rising -> [%rope "posedge"]
      | Falling -> [%rope "negedge"]
    in
    let clock = proposition clock in
    let clocked = [%rope "@(%{edge} %{clock})"] in
    match disable_iff with
    | None -> clocked
    | Some disable_iff ->
      let disable_iff = proposition disable_iff in
      [%rope "%{clocked} disable iff (%{disable_iff})"]
  ;;

  let atomic_propositions { edge = _; clock; disable_iff } =
    match disable_iff with
    | None -> [ clock ]
    | Some disable_iff -> [ clock; disable_iff ]
  ;;
end

type ('var, 'formula) property =
  { directive : Directive.t
  ; clocked : 'var Clocked.t option
  ; formula : 'formula
  }
[@@deriving sexp_of]

let atomic_proposition_must_be_1_bit ap =
  if Signal.width ap <> 1
  then raise_s [%message "atomic propositions must be 1 bit" (ap : Signal.t)]
;;

module CTL = struct
  module Formula = struct
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

    let t = True

    let p ap =
      atomic_proposition_must_be_1_bit ap;
      P ap
    ;;

    let ( &: ) a b = And (a, b)
    let ( ~: ) a = Not a
    let e p = E p
    let a p = A p
    let x p = X p
    let rec ax ?(n = 1) s = if n = 0 then s else a (x (ax ~n:(n - 1) s))
    let rec ex ?(n = 1) s = if n = 0 then s else e (x (ex ~n:(n - 1) s))
    let u a b = U (a, b)
    let au x y = a @@ u x y
    let eu x y = e @@ u x y
    let f s = F s
    let af s = a @@ f s
    let ef s = e @@ f s
    let g s = G s
    let ag s = a @@ g s
    let eg s = e @@ g s

    let rec to_string ~proposition p =
      let to_string = to_string ~proposition in
      match p with
      | True -> "TRUE"
      | P ap -> proposition ap
      | And (a, b) -> "(" ^ to_string a ^ " & " ^ to_string b ^ ")"
      | Not s -> "(!" ^ to_string s ^ ")"
      | E (G p) -> "(EG " ^ to_string p ^ ")"
      | E (F p) -> "(EF " ^ to_string p ^ ")"
      | E (X p) -> "(EX " ^ to_string p ^ ")"
      | E (U (a, b)) -> "(E [" ^ to_string a ^ " U " ^ to_string b ^ "])"
      | A (G p) -> "(AG " ^ to_string p ^ ")"
      | A (F p) -> "(AF " ^ to_string p ^ ")"
      | A (X p) -> "(AX " ^ to_string p ^ ")"
      | A (U (a, b)) -> "(A [" ^ to_string a ^ " U " ^ to_string b ^ "])"
    ;;

    let rec atomic_propositions = function
      | True -> []
      | P ap -> [ ap ]
      | And (a, b) -> atomic_propositions a @ atomic_propositions b
      | Not s -> atomic_propositions s
      | E (G p) -> atomic_propositions p
      | E (F p) -> atomic_propositions p
      | E (X p) -> atomic_propositions p
      | E (U (a, b)) -> atomic_propositions a @ atomic_propositions b
      | A (G p) -> atomic_propositions p
      | A (F p) -> atomic_propositions p
      | A (X p) -> atomic_propositions p
      | A (U (a, b)) -> atomic_propositions a @ atomic_propositions b
    ;;

    let map_atomic_propositions p ~f =
      let rec g = function
        | True -> True
        | P ap -> P (f ap)
        | And (a, b) -> And (g a, g b)
        | Not s -> Not (g s)
        | E (G p) -> E (G (g p))
        | E (F p) -> E (F (g p))
        | E (X p) -> E (X (g p))
        | E (U (a, b)) -> E (U (g a, g b))
        | A (G p) -> A (G (g p))
        | A (F p) -> A (F (g p))
        | A (X p) -> A (X (g p))
        | A (U (a, b)) -> A (U (g a, g b))
      in
      g p
    ;;
  end
end

module LTL = struct
  module Formula = struct
    module Unstable = struct
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
      [@@deriving sexp, bin_io]
    end

    include Unstable

    let vdd = True
    let gnd = Not True
    let atom v = P v

    let p ap =
      atomic_proposition_must_be_1_bit ap;
      P ap
    ;;

    let ( &: ) a b = And (a, b)
    let ( |: ) a b = Or (a, b)
    let ( ~: ) a = Not a
    let ( ^: ) a b = a &: ~:b |: (~:a &: b)
    let ( ==: ) a b = ~:(a ^: b)
    let ( <>: ) a b = a ^: b
    let ( ==>: ) a b = ~:a |: b
    let rec x ?(n = 1) s = if n = 0 then s else X (x ~n:(n - 1) s)
    let u a b = U (a, b)
    let r a b = R (a, b)

    (* let f p = vdd -- p let g p = ~: (f (~: p)) *)
    let f p = F p
    let g p = G p
    let w p q = u p q |: g p

    let rec to_string ~proposition p =
      let to_string = to_string ~proposition in
      match p with
      | U (True, b) -> "(F " ^ to_string b ^ ")"
      | Not (U (True, Not p)) -> "(G " ^ to_string p ^ ")"
      | True -> "TRUE"
      | P ap -> proposition ap
      | Pn ap -> "(!" ^ to_string (P ap) ^ ")"
      | And (a, b) -> "(" ^ to_string a ^ " & " ^ to_string b ^ ")"
      | Or (a, b) -> "(" ^ to_string a ^ " | " ^ to_string b ^ ")"
      | Not a -> "(!" ^ to_string a ^ ")"
      | X p -> "(X " ^ to_string p ^ ")"
      | U (a, b) -> "(" ^ to_string a ^ " U " ^ to_string b ^ ")"
      | R (a, b) -> "(" ^ to_string a ^ " V " ^ to_string b ^ ")"
      | F p -> "(F " ^ to_string p ^ ")"
      | G p -> "(G " ^ to_string p ^ ")"
    ;;

    let rec atomic_propositions = function
      | True -> []
      | P ap -> [ ap ]
      | Pn ap -> [ ap ]
      | And (a, b) -> atomic_propositions a @ atomic_propositions b
      | Or (a, b) -> atomic_propositions a @ atomic_propositions b
      | Not a -> atomic_propositions a
      | X p -> atomic_propositions p
      | U (a, b) -> atomic_propositions a @ atomic_propositions b
      | R (a, b) -> atomic_propositions a @ atomic_propositions b
      | F p -> atomic_propositions p
      | G p -> atomic_propositions p
    ;;

    let map_atomic_propositions p ~f =
      let rec g = function
        | True -> True
        | P ap -> P (f ap)
        | Pn ap -> Pn (f ap)
        | And (a, b) -> And (g a, g b)
        | Or (a, b) -> Or (g a, g b)
        | Not a -> Not (g a)
        | X p -> X (g p)
        | U (a, b) -> U (g a, g b)
        | R (a, b) -> R (g a, g b)
        | F p -> F (g p)
        | G p -> G (g p)
      in
      g p
    ;;

    let rec depth = function
      | True -> 0
      | P _ -> 0
      | Pn _ -> 0
      | And (a, b) -> max (depth a) (depth b)
      | Or (a, b) -> max (depth a) (depth b)
      | Not a -> depth a
      | X p -> 1 + depth p
      | U (a, b) -> max (depth a) (depth b)
      | R (a, b) -> max (depth a) (depth b)
      | F p -> depth p
      | G p -> depth p
    ;;

    (*=demorgan's law: ~(a & b) = (~a | ~b)
     ~(a | b) = (~a & ~b)

     without an OR primitive we must then expand with (a | b) = ~( ~a & ~b )
     which gives us back a not gate (undoes the initial simplification).

     Similarly using 'r' defined in terms of 'u' *)
    let rec nnf x =
      match x with
      (* positive *)
      | True | P _ -> x
      | Pn _ -> x
      | And (a, b) -> And (nnf a, nnf b)
      | Or (a, b) -> Or (nnf a, nnf b)
      | X a -> X (nnf a)
      | U (a, b) -> U (nnf a, nnf b)
      | R (a, b) -> R (nnf a, nnf b)
      | F a -> F (nnf a)
      | G a -> G (nnf a)
      (* negative *)
      | Not True -> x
      | Not (P x) -> Pn x
      | Not (Pn x) -> P x
      | Not (And (a, b)) -> nnf ~:a |: nnf ~:b (* demorgans *)
      | Not (Or (a, b)) -> nnf ~:a &: nnf ~:b
      | Not (Not a) -> nnf a
      | Not (X p) -> X (nnf (Not p))
      | Not (U (a, b)) -> R (nnf ~:a, nnf ~:b)
      | Not (R (a, b)) -> U (nnf ~:a, nnf ~:b)
      | Not (F a) -> G (nnf ~:a)
      | Not (G a) -> F (nnf ~:a)
    ;;

    let limit_depth k x =
      let rec f i x =
        match x with
        | X p -> if i < k then f (i + 1) p else Not True
        | True | P _ | Pn _ -> x
        | And (a, b) -> And (f i a, f i b)
        | Or (a, b) -> Or (f i a, f i b)
        | U (a, b) -> U (f i a, f i b)
        | R (a, b) -> R (f i a, f i b)
        | F a -> F (f i a)
        | G a -> G (f i a)
        | Not a -> Not (f i a)
      in
      f 0 x
    ;;
  end

  type t = (Signal.t, Signal.t Formula.t) property [@@deriving sexp_of]

  let create ?clocked directive formula = { directive; clocked; formula }
  let asserts ?clocked formula = { directive = Assert; clocked; formula }
  let assume ?clocked formula = { directive = Assume; clocked; formula }
  let cover ?clocked formula = { directive = Cover; clocked; formula }
end

module Sva = struct
  module Formula = struct
    module Count = struct
      type t = int [@@deriving sexp_of]

      let to_string t = "[" ^ Int.to_string t ^ "]"
    end

    module Range = struct
      type t =
        { from : int
        ; to_ : int
        }
      [@@deriving sexp_of]

      let to_string { from; to_ } =
        String.concat [ "["; Int.to_string from; ":"; Int.to_string to_; "]" ]
      ;;
    end

    (* These type witnesses track what kind of formula this is. A sequence can be used as
       either a sequence or a property; a property can only be used as a property. *)

    module Sequence_range = struct
      type t =
        | Bounded of Range.t
        | At_least of int
        | At_most of int
      [@@deriving sexp_of]

      let require_non_negative n =
        if n < 0
        then raise_s [%message "SVA sequence range must be non-negative" (n : int)]
      ;;

      let bounded_exn ({ from; to_ } as range : Range.t) =
        require_non_negative from;
        require_non_negative to_;
        if from > to_
        then raise_s [%message "SVA sequence range must be increasing" (range : Range.t)];
        Bounded range
      ;;

      let at_least_exn n =
        require_non_negative n;
        At_least n
      ;;

      let at_most_exn n =
        require_non_negative n;
        At_most n
      ;;

      let to_delay_string t =
        match t with
        | Bounded range -> Range.to_string range
        | At_least n -> "[" ^ Int.to_string n ^ ":$]"
        | At_most n -> "[0:" ^ Int.to_string n ^ "]"
      ;;

      let to_repeat_string t =
        match t with
        | Bounded { from; to_ } ->
          "[*" ^ Int.to_string from ^ ":" ^ Int.to_string to_ ^ "]"
        | At_least 0 -> "[*]"
        | At_least 1 -> "[+]"
        | At_least n -> "[*" ^ Int.to_string n ^ ":$]"
        | At_most n -> "[*0:" ^ Int.to_string n ^ "]"
      ;;
    end

    type sequence =
      [ `Sequence
      | `Property
      ]
    [@@deriving sexp_of]

    type property = [ `Property ] [@@deriving sexp_of]

    module Kind_witness = struct
      type _ t =
        | Sequence : sequence t
        | Property : property t
    end

    type ('var, 'kind) t =
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
      | And : ('var, 'kind) t * ('var, 'kind) t -> ('var, 'kind) t
      | Or : ('var, 'kind) t * ('var, 'kind) t -> ('var, 'kind) t
      | Not : ('var, [> property ]) t -> ('var, property) t
      | Property_implication :
          ('var, [> property ]) t * ('var, [> property ]) t
          -> ('var, property) t
      | Iff : ('var, [> property ]) t * ('var, [> property ]) t -> ('var, property) t
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

    module For_sexp_of = struct
      type 'var t =
        | True
        | Atom of 'var
        | Delay of 'var t option * Sequence_range.t * 'var t
        | Repeat of 'var t * Sequence_range.t
        | Throughout of 'var * 'var t
        | Overlapping_implication of 'var t * 'var t
        | And of 'var t * 'var t
        | Or of 'var t * 'var t
        | Not of 'var t
        | Property_implication of 'var t * 'var t
        | Iff of 'var t * 'var t
        | Nexttime of 'var t * Count.t option
        | S_nexttime of 'var t * Count.t option
        | Always of 'var t * Range.t option
        | S_always of 'var t * Range.t
        | Eventually of 'var t * Range.t
        | S_eventually of 'var t * Range.t option
        | Until of 'var t * 'var t
        | Until_with of 'var t * 'var t
        | S_until of 'var t * 'var t
        | S_until_with of 'var t * 'var t
        | Sequence_as_property of 'var t
      [@@deriving sexp_of]
    end

    let sexp_of_t (type var kind) (sexp_of_var : var -> Sexp.t) _ (t : (var, kind) t) =
      let rec to_for_sexp_of : type kind. (var, kind) t -> var For_sexp_of.t = function
        | True -> True
        | Atom v -> Atom v
        | Delay (a, range, b) ->
          Delay (Option.map a ~f:to_for_sexp_of, range, to_for_sexp_of b)
        | Repeat (t, range) -> Repeat (to_for_sexp_of t, range)
        | Throughout (v, t) -> Throughout (v, to_for_sexp_of t)
        | Overlapping_implication (a, b) ->
          Overlapping_implication (to_for_sexp_of a, to_for_sexp_of b)
        | And (a, b) -> And (to_for_sexp_of a, to_for_sexp_of b)
        | Or (a, b) -> Or (to_for_sexp_of a, to_for_sexp_of b)
        | Not t -> Not (to_for_sexp_of t)
        | Property_implication (a, b) ->
          Property_implication (to_for_sexp_of a, to_for_sexp_of b)
        | Iff (a, b) -> Iff (to_for_sexp_of a, to_for_sexp_of b)
        | Nexttime (t, count) -> Nexttime (to_for_sexp_of t, count)
        | S_nexttime (t, count) -> S_nexttime (to_for_sexp_of t, count)
        | Always (t, range) -> Always (to_for_sexp_of t, range)
        | S_always (t, range) -> S_always (to_for_sexp_of t, range)
        | Eventually (t, range) -> Eventually (to_for_sexp_of t, range)
        | S_eventually (t, range) -> S_eventually (to_for_sexp_of t, range)
        | Until (a, b) -> Until (to_for_sexp_of a, to_for_sexp_of b)
        | Until_with (a, b) -> Until_with (to_for_sexp_of a, to_for_sexp_of b)
        | S_until (a, b) -> S_until (to_for_sexp_of a, to_for_sexp_of b)
        | S_until_with (a, b) -> S_until_with (to_for_sexp_of a, to_for_sexp_of b)
        | Sequence_as_property t -> Sequence_as_property (to_for_sexp_of t)
      in
      [%sexp_of: var For_sexp_of.t] (to_for_sexp_of t)
    ;;

    let rec kind : type var kind. (var, kind) t -> kind Kind_witness.t = function
      | True -> Sequence
      | Atom (_ : var) -> Sequence
      | Delay
          ((_ : (var, sequence) t option), (_ : Sequence_range.t), (_ : (var, sequence) t))
        -> Sequence
      | Repeat ((_ : (var, sequence) t), (_ : Sequence_range.t)) -> Sequence
      | Throughout ((_ : var), (_ : (var, sequence) t)) -> Sequence
      | Overlapping_implication ((_ : (var, sequence) t), (_ : (var, property) t)) ->
        Property
      | And (a, (_ : (var, kind) t)) -> kind a
      | Or (a, (_ : (var, kind) t)) -> kind a
      | Not (_ : (var, [> property ]) t) -> Property
      | Property_implication ((_ : (var, [> property ]) t), (_ : (var, [> property ]) t))
        -> Property
      | Iff ((_ : (var, [> property ]) t), (_ : (var, [> property ]) t)) -> Property
      | Nexttime ((_ : (var, [> property ]) t), (_ : Count.t option)) -> Property
      | S_nexttime ((_ : (var, [> property ]) t), (_ : Count.t option)) -> Property
      | Always ((_ : (var, [> property ]) t), (_ : Range.t option)) -> Property
      | S_always ((_ : (var, [> property ]) t), (_ : Range.t)) -> Property
      | Eventually ((_ : (var, [> property ]) t), (_ : Range.t)) -> Property
      | S_eventually ((_ : (var, [> property ]) t), (_ : Range.t option)) -> Property
      | Until ((_ : (var, [> property ]) t), (_ : (var, [> property ]) t)) -> Property
      | Until_with ((_ : (var, [> property ]) t), (_ : (var, [> property ]) t)) ->
        Property
      | S_until ((_ : (var, [> property ]) t), (_ : (var, [> property ]) t)) -> Property
      | S_until_with ((_ : (var, [> property ]) t), (_ : (var, [> property ]) t)) ->
        Property
      | Sequence_as_property (_ : (var, sequence) t) -> Property
    ;;

    let as_property (type var kind) (t : (var, kind) t) : (var, property) t =
      match kind t with
      | Sequence -> Sequence_as_property t
      | Property -> t
    ;;

    let of_ltl (type var) (ltl : var LTL.Formula.t) =
      let ebmc_bugs = true in
      let rec f (path : var LTL.Formula.t) : (var, [> property ]) t =
        match path with
        | True -> Sequence_as_property True
        | P v -> Sequence_as_property (Atom v)
        | Pn v -> Not (f (P v))
        | Not p -> Not (f p)
        | And (p, q) -> And (f p, f q)
        | Or (p, q) -> Or (f p, f q)
        | X p -> Nexttime (f p, None)
        | R (cond, inv) ->
          if ebmc_bugs
          then (
            let inv = f inv in
            Until (inv, And (inv, f cond)))
          else Until_with (f inv, f cond)
        | U (inv, cond) ->
          if ebmc_bugs
          then (
            let cond = f cond in
            And (Until (f inv, cond), S_eventually (cond, None)))
          else S_until (f inv, f cond)
        | F p -> S_eventually (f p, None)
        | G p -> Always (f p, None)
      in
      f (LTL.Formula.nnf ltl)
    ;;

    let to_rope (type var) ~(proposition : var -> Rope.t) =
      let rec to_rope : type kind. (var, kind) t -> Rope.t = function
        | True -> [%rope "1'b1"]
        | Atom v -> proposition v
        | Not p -> [%rope "not (%{to_rope p})"]
        | And (p, q) -> [%rope "(%{to_rope p}) and (%{to_rope q})"]
        | Or (p, q) -> [%rope "(%{to_rope p}) or (%{to_rope q})"]
        | Delay (None, range, p) ->
          let range = Sequence_range.to_delay_string range |> Rope.of_string in
          [%rope "##%{range} (%{to_rope p})"]
        | Delay (Some p, range, q) ->
          let range = Sequence_range.to_delay_string range |> Rope.of_string in
          [%rope "(%{to_rope p}) ##%{range} (%{to_rope q})"]
        | Repeat (p, range) ->
          let range = Sequence_range.to_repeat_string range |> Rope.of_string in
          [%rope "(%{to_rope p}) %{range}"]
        | Throughout (v, p) -> [%rope "(%{proposition v}) throughout (%{to_rope p})"]
        | Overlapping_implication (p, q) -> [%rope "(%{to_rope p}) |-> (%{to_rope q})"]
        | Nexttime (p, None) -> [%rope "nexttime (%{to_rope p})"]
        | Nexttime (p, Some count) -> [%rope "nexttime %{count#Count} (%{to_rope p})"]
        | S_nexttime (p, None) -> [%rope "s_nexttime (%{to_rope p})"]
        | S_nexttime (p, Some count) -> [%rope "s_nexttime %{count#Count} (%{to_rope p})"]
        | Always (p, None) -> [%rope "always (%{to_rope p})"]
        | Always (p, Some range) -> [%rope "always %{range#Range} (%{to_rope p})"]
        | S_always (p, range) -> [%rope "s_always %{range#Range} (%{to_rope p})"]
        | Eventually (p, range) -> [%rope "eventually %{range#Range} (%{to_rope p})"]
        | S_eventually (p, None) -> [%rope "s_eventually (%{to_rope p})"]
        | S_eventually (p, Some range) ->
          [%rope "s_eventually %{range#Range} (%{to_rope p})"]
        | S_until (inv, cond) -> [%rope "(%{to_rope inv}) s_until (%{to_rope cond})"]
        | Until (inv, cond) -> [%rope "(%{to_rope inv}) until (%{to_rope cond})"]
        | Until_with (inv, cond) ->
          [%rope "(%{to_rope inv}) until_with (%{to_rope cond})"]
        | S_until_with (inv, cond) ->
          [%rope "(%{to_rope inv}) s_until_with (%{to_rope cond})"]
        | Iff (p, q) -> [%rope "(%{to_rope p}) iff (%{to_rope q})"]
        | Property_implication (p, q) -> [%rope "(%{to_rope p}) implies (%{to_rope q})"]
        | Sequence_as_property p -> to_rope p
      in
      to_rope
    ;;

    let rec atomic_propositions : type var kind. (var, kind) t -> var list = function
      | True -> []
      | Atom v -> [ v ]
      | Delay (None, _, t) -> atomic_propositions t
      | Delay (Some a, _, b) -> atomic_propositions a @ atomic_propositions b
      | Repeat (t, _) -> atomic_propositions t
      | Throughout (v, t) -> v :: atomic_propositions t
      | Overlapping_implication (a, b) -> atomic_propositions a @ atomic_propositions b
      | Not t -> atomic_propositions t
      | And (a, b) -> atomic_propositions a @ atomic_propositions b
      | Or (a, b) -> atomic_propositions a @ atomic_propositions b
      | Nexttime (t, _) -> atomic_propositions t
      | S_nexttime (t, _) -> atomic_propositions t
      | Always (t, _) -> atomic_propositions t
      | S_always (t, _) -> atomic_propositions t
      | Eventually (t, _) -> atomic_propositions t
      | S_eventually (t, _) -> atomic_propositions t
      | Until (a, b) -> atomic_propositions a @ atomic_propositions b
      | Until_with (a, b) -> atomic_propositions a @ atomic_propositions b
      | S_until (a, b) -> atomic_propositions a @ atomic_propositions b
      | S_until_with (a, b) -> atomic_propositions a @ atomic_propositions b
      | Iff (a, b) -> atomic_propositions a @ atomic_propositions b
      | Property_implication (a, b) -> atomic_propositions a @ atomic_propositions b
      | Sequence_as_property a -> atomic_propositions a
    ;;

    let map_atomic_propositions (type a b kind) (t : (a, kind) t) ~(f : a -> b)
      : (b, kind) t
      =
      let rec map_atomic_propositions : type kind. (a, kind) t -> (b, kind) t = function
        | True -> True
        | Atom v -> Atom (f v)
        | Delay (None, range, t) -> Delay (None, range, map_atomic_propositions t)
        | Delay (Some a, range, b) ->
          Delay (Some (map_atomic_propositions a), range, map_atomic_propositions b)
        | Repeat (t, range) -> Repeat (map_atomic_propositions t, range)
        | Throughout (v, t) -> Throughout (f v, map_atomic_propositions t)
        | Overlapping_implication (a, b) ->
          Overlapping_implication (map_atomic_propositions a, map_atomic_propositions b)
        | Not t -> Not (map_atomic_propositions t)
        | And (a, b) -> And (map_atomic_propositions a, map_atomic_propositions b)
        | Or (a, b) -> Or (map_atomic_propositions a, map_atomic_propositions b)
        | Nexttime (t, count) -> Nexttime (map_atomic_propositions t, count)
        | S_nexttime (t, count) -> S_nexttime (map_atomic_propositions t, count)
        | Always (t, range) -> Always (map_atomic_propositions t, range)
        | S_always (t, range) -> S_always (map_atomic_propositions t, range)
        | Eventually (t, range) -> Eventually (map_atomic_propositions t, range)
        | S_eventually (t, range) -> S_eventually (map_atomic_propositions t, range)
        | Until (a, b) -> Until (map_atomic_propositions a, map_atomic_propositions b)
        | Until_with (a, b) ->
          Until_with (map_atomic_propositions a, map_atomic_propositions b)
        | S_until (a, b) -> S_until (map_atomic_propositions a, map_atomic_propositions b)
        | S_until_with (a, b) ->
          S_until_with (map_atomic_propositions a, map_atomic_propositions b)
        | Iff (a, b) -> Iff (map_atomic_propositions a, map_atomic_propositions b)
        | Property_implication (a, b) ->
          Property_implication (map_atomic_propositions a, map_atomic_propositions b)
        | Sequence_as_property a -> Sequence_as_property (map_atomic_propositions a)
      in
      map_atomic_propositions t
    ;;
  end

  type 'var t = ('var, ('var, Formula.property) Formula.t) property [@@deriving sexp_of]

  let create ?clocked directive formula =
    { directive; clocked; formula = Formula.as_property formula }
  ;;

  let asserts ?clocked formula =
    { directive = Assert; clocked; formula = Formula.as_property formula }
  ;;

  let assume ?clocked formula =
    { directive = Assume; clocked; formula = Formula.as_property formula }
  ;;

  let cover ?clocked formula =
    { directive = Cover; clocked; formula = Formula.as_property formula }
  ;;

  let map_atomic_propositions t ~f =
    { t with
      clocked = Option.map t.clocked ~f:(Clocked.map ~f)
    ; formula = Formula.map_atomic_propositions t.formula ~f
    }
  ;;

  let atomic_propositions t =
    List.concat
      [ Option.value_map t.clocked ~default:[] ~f:Clocked.atomic_propositions
      ; Formula.atomic_propositions t.formula
      ]
  ;;

  let to_rope ~proposition t =
    let formula = Formula.to_rope ~proposition t.formula in
    match t.clocked with
    | None -> [%rope "%{t.directive#Directive} property (%{formula})"]
    | Some clocked ->
      let clocked = Clocked.to_rope ~proposition clocked in
      [%rope "%{t.directive#Directive} property (%{clocked} %{formula})"]
  ;;

  let of_ltl ltl = { ltl with formula = Formula.of_ltl ltl.formula }
end
