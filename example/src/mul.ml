open! Import

include Mul_intf

module Config = struct
  type t =
    | Dadda
    | Wallace
  [@@deriving enumerate, sexp_of]
end

module Make_gen (B : Gen) = struct

  open B

  module A = Add.Make_gen (B)

  (* A [Weights.t] represents the product of two [B.t]s as a set of weighted single-bit
     wires, where the value of a wire is zero if its bit is zero, or [Int.pow 2 w] if its
     bit is one, where [w] is the weight of the wire.  [Weights.t] stores the wires
     indexed by weight. *)
  module Weights : sig
    type t [@@deriving sexp_of]

    (* [create a b] creates the trivially correct product by multiplying every bit
       in [a] by every bit in [b]. *)
    val create : B.t -> B.t -> t

    val max_wires_at_any_weight : t -> int

    (* [layer t ~config] does one round of simplification on [t] by combining groups of
       three wires of the same weight using a full adder, and possibly combining leftover
       groups of two wires of the same weight using a half adder, as per [config]. *)
    val layer : t -> config:Config.t -> t

    (* [sum t] should only be called if [max_wires_at_any_weight t <= 2].  [sum]
       constructs two numbers out of the wires in [t] and uses [B.(+:)] to add them. *)
    val sum : t -> target_width:int -> B.t
  end = struct
    (* [t.(w)] holds all wires of weight [w]. *)
    type t = B.bit list array [@@deriving sexp_of]

    let max_wires_at_any_weight t =
      Array.fold t ~init:0 ~f:(fun max wires -> Int.max max (List.length wires))

    let create a b =
      let wa = width a in
      let wb = width b in
      let max_weight = wa + wb - 2 in
      Array.init (max_weight + 1) ~f:(fun w ->
        List.init wa ~f:(fun i ->
          List.init wb ~f:(fun j ->
            if i+j = w
            then Some (bit a i &: bit b j)
            else None))
        |> List.concat
        |> List.filter_opt)

    let layer (t : t) ~(config : Config.t) =
      let is_final_stage = max_wires_at_any_weight t = 3 in
      let result = Array.create ~len:(Array.length t + 1) [] in
      let add weight wire = result.( weight ) <- wire :: result.( weight ) in
      Array.iteri t ~f:(fun weight wires ->
        let rec loop wires =
          match wires with
          | [] -> ()
          | [ a ] -> add weight a
          | [ a; b ] ->
            let use_half_adder =
              match config with
              | Wallace -> true
              | Dadda ->
                (* We're trying to make [length result.(weight)] be zero mod 3 after
                   including [a] and [b].  [m] measures the current length mod 3, not
                   including [a] and [b].  So if [m = 2], we use a half adder, which will
                   make the length be zero mod 3 after including the [sum] output of the
                   half adder. *)
                let m = List.length result.( weight ) % 3 in
                if is_final_stage then m <> 0 else m = 2 in
            if use_half_adder
            then (
              let { A. carry; sum } = A.half_adder a b in
              add weight sum;
              add (weight + 1) carry)
            else (
              add weight a;
              add weight b)
          | a :: b :: c :: wires ->
            let { A. carry; sum } = A.full_adder a b c in
            add weight sum;
            add (weight + 1) carry;
            loop wires in
        loop wires);
      result

    let sum t ~target_width =
      let a, b =
        Array.fold t ~init:([], []) ~f:(fun (a, b) ab ->
          let a', b' =
            match ab with
            | [ a'; b' ] -> a', b'
            | [ a'     ] -> a', gnd
            | [        ] -> gnd, gnd
            | _ -> assert false in
          a' :: a, b' :: b) in
      uresize (concat a) target_width
      +: uresize (concat b) target_width
  end

  let create config a b =
    let rec optimise (weights : Weights.t) =
      let max_wires_at_any_weight = Weights.max_wires_at_any_weight weights in
      if max_wires_at_any_weight <= 2
      then weights
      else optimise (Weights.layer weights ~config)
    in
    Weights.sum (optimise (Weights.create a b)) ~target_width:(width a + width b)
end

let create_gen (type a) ~(config : Config.t) (module B : Gen with type t = a) a b =
  let module M = Make_gen (B) in
  M.create config a b

module Comb_as_gen (B : Comb.S)
  : Gen
    with type t = B.t
    with type bit = B.t = struct
  include B
  type bit = t [@@deriving sexp_of]
end

let create
      (type a)
      ~config
      (module B : Comb.S with type t = a)
      a b =
  create_gen ~config (module Comb_as_gen (B)) a b
