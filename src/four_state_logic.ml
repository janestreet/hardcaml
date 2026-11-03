open! Core0

type t =
  { bits : Bits.t
  ; mask : Bits.t
  }
[@@deriving bin_io, equal, compare, fields ~getters]

let create ~bits ~mask =
  match Int.equal (Bits.width bits) (Bits.width mask) with
  | true -> Ok { bits; mask }
  | false ->
    Or_error.error_s
      [%message
        "[Four_state_logic.create] planes have different widths"
          ~bits_width:(Bits.width bits : int)
          ~mask_width:(Bits.width mask : int)]
;;

let create_exn ~bits ~mask = create ~bits ~mask |> Or_error.ok_exn
let ( = ) = equal

let to_string { bits; mask } =
  let bits = Bits.to_bstr bits in
  let mask = Bits.to_bstr mask in
  String.mapi bits ~f:(fun index bit ->
    match mask.[index], bit with
    | '0', bit -> bit
    | '1', '1' -> 'X'
    | '1', '0' -> 'Z'
    | mask, bit -> raise_s [%message "Unexpected bit string" (mask : char) (bit : char)])
;;

let sexp_of_t t = Sexp.Atom (to_string t)

module Impurity = struct
  type t =
    | Pure
    | X
    | Z
  [@@deriving equal, sexp_of]
end

let impurity { bits; mask } : Impurity.t =
  match Bits.to_bool (Bits.any_bit_set mask) with
  | false -> Pure
  | true -> if Bits.to_bool (Bits.any_bit_set (Bits.( &: ) bits mask)) then X else Z
;;

let is_pure t =
  match impurity t with
  | Pure -> true
  | X | Z -> false
;;

let of_bits t = { bits = t; mask = Bits.zero (Bits.width t) }

let to_bits_exn t =
  if not (is_pure t)
  then raise_s [%message "cannot convert to bits - value has error bits set" (t : t)];
  t.bits
;;

let plane_chars : Logic.Four_state.t -> char * char = function
  | L0 -> '0', '0'
  | L1 -> '1', '0'
  | X -> '1', '1'
  | Z -> '0', '1'
;;

let high_impedance width = { bits = Bits.zero width; mask = Bits.ones width }
let don't_care width = { bits = Bits.ones width; mask = Bits.ones width }
let undefined = don't_care

module Make_comb (C : sig
    val pessimistic : bool
  end) =
struct
  module Base = struct
    type nonrec t = t [@@deriving equal]

    let to_string = to_string
    let sexp_of_t = sexp_of_t
    let empty = { bits = Bits.empty; mask = Bits.empty }
    let is_empty { bits; _ } = Bits.is_empty bits
    let width { bits; _ } = Bits.width bits
    let of_constant c = of_bits (Bits.of_constant c)
    let to_constant t = Bits.to_constant (to_bits_exn t)
    let vdd = of_constant (Constant.of_int ~width:1 1)
    let gnd = of_constant (Constant.of_int ~width:1 0)

    let concat_msb l =
      { bits = Bits.concat_msb (List.map l ~f:bits)
      ; mask = Bits.concat_msb (List.map l ~f:mask)
      }
    ;;

    let select t ~high ~low =
      { bits = Bits.select t.bits ~high ~low; mask = Bits.select t.mask ~high ~low }
    ;;

    let ( -- ) ?(loc = Stdlib.Lexing.dummy_pos) t _name =
      ignore loc;
      t
    ;;

    let[@inline] op1 op a =
      let open Bits in
      let mask = a.mask in
      { bits = op a.bits |: mask; mask }
    ;;

    let[@inline] op2 op a b =
      let open Bits in
      let mask = a.mask |: b.mask in
      { bits = op a.bits b.bits |: mask; mask }
    ;;

    let pessimistic_and = op2 Bits.( &: )
    let pessimistic_or = op2 Bits.( |: )
    let bit_is_0 { bits; mask } = Bits.(~:bits &: ~:mask)
    let bit_is_1 { bits; mask } = Bits.(bits &: ~:mask)

    let optimistic_and a b =
      (* if either bit is 0 the result is 0 *)
      let open Bits in
      let is_0 = bit_is_0 a |: bit_is_0 b in
      let is_1 = bit_is_1 a &: bit_is_1 b in
      let is_impure = ~:(is_0 |: is_1) in
      { bits = is_1 |: is_impure; mask = is_impure }
    ;;

    let optimistic_or (a : t) (b : t) =
      (* if either bit is 1 the result is 1 *)
      let open Bits in
      let is_0 = bit_is_0 a &: bit_is_0 b in
      let is_1 = bit_is_1 a |: bit_is_1 b in
      let is_impure = ~:(is_0 |: is_1) in
      { bits = is_1 |: is_impure; mask = is_impure }
    ;;

    let ( &: ) = if C.pessimistic then pessimistic_and else optimistic_and
    let ( |: ) = if C.pessimistic then pessimistic_or else optimistic_or
    let ( ^: ) = op2 Bits.( ^: )
    let ( ~: ) = op1 Bits.( ~: )

    type op2_output_width =
      | Same
      | Sum
      | One

    (* Note; there aren't any optimistic versions of these operations.

       They can be defined optimistically at the gate level, but that doesn't match any
       known simulator behavior. *)
    let[@inline] pessimistic_op2 op op2_output_width a b =
      if is_pure a && is_pure b
      then (
        let bits = op a.bits b.bits in
        { bits; mask = Bits.zero (Bits.width bits) })
      else (
        match op2_output_width with
        | Same -> undefined (Bits.width a.bits)
        | Sum -> undefined (Bits.width a.bits + Bits.width b.bits)
        | One -> undefined 1)
    ;;

    let ( +: ) = pessimistic_op2 Bits.( +: ) Same
    let ( -: ) = pessimistic_op2 Bits.( -: ) Same
    let ( *: ) = pessimistic_op2 Bits.( *: ) Sum
    let ( *+ ) = pessimistic_op2 Bits.( *+ ) Sum
    let ( <: ) = pessimistic_op2 Bits.( <: ) One
    let pessimistic_eq = pessimistic_op2 Bits.( ==: ) One

    let optimistic_eq a b =
      let impure = Bits.(a.mask |: b.mask) in
      let any_impure = Bits.any_bit_set impure in
      let definite_mismatch = Bits.(any_bit_set (a.bits ^: b.bits &: ~:impure)) in
      if Bits.to_bool definite_mismatch
      then gnd
      else if Bits.to_bool any_impure
      then don't_care 1
      else vdd
    ;;

    let ( ==: ) = if C.pessimistic then pessimistic_eq else optimistic_eq

    let mux sel data =
      if is_pure sel
      then (
        let len = List.length data in
        let index = min (len - 1) (Bits.to_unsigned_int sel.bits) in
        List.nth_exn data index)
      else (
        let width = Bits.width (List.hd_exn data).bits in
        don't_care width)
    ;;

    include Comb.Expert.Gen_cases_from_mux (struct
        type nonrec t = t

        let mux = mux
        let ( ==: ) = ( ==: )
      end)
  end

  include Comb.Make (Base)

  let of_bits_string string =
    let plane ~f =
      String.map string ~f:(fun char ->
        Logic.Four_state.of_char_exn char |> plane_chars |> f)
      |> Bits.of_string
    in
    { bits = plane ~f:fst; mask = plane ~f:snd }
  ;;

  (* Override the default hardcaml 2-state version. If 2 state parsing fails (including
     things like 23'd101), then try as a 4 state bit string. *)
  let of_string string =
    try of_string string with
    | _ -> of_bits_string string
  ;;
end

module Pessimistic = Make_comb (struct
    let pessimistic = true
  end)

module Optimistic = Make_comb (struct
    let pessimistic = false
  end)

include (Pessimistic : Comb.S with type t := t)
