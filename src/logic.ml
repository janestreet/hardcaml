open Base

module type Std_logic = Logic_intf.Std_logic
module type Four_state = Logic_intf.Four_state

module Std_logic = struct
  type t =
    | U
    | X
    | L0
    | L1
    | Z
    | W
    | L
    | H
    | Don't_care
  [@@deriving compare, enumerate, sexp, variants]

  let optimise_muxs = false
  let constant_only = true
  let equal = [%compare.equal: t]

  let to_char = function
    | U -> 'U'
    | X -> 'X'
    | L0 -> '0'
    | L1 -> '1'
    | Z -> 'Z'
    | W -> 'W'
    | L -> 'L'
    | H -> 'H'
    | Don't_care -> '_'
  ;;

  let of_char = function
    | 'U' -> U
    | 'X' -> X
    | '0' -> L0
    | '1' -> L1
    | 'Z' -> Z
    | 'W' -> W
    | 'L' -> L
    | 'H' -> H
    | '_' -> Don't_care
    | _ -> raise_s [%message "invalid char"]
  ;;

  let sexp_of_t t = [%sexp (to_char t : char)]
  let to_int = Variants.to_rank

  let of_char_exn = function
    | 'U' | 'u' -> U
    | 'X' | 'x' -> X
    | '0' -> L0
    | '1' -> L1
    | 'Z' | 'z' -> Z
    | 'W' | 'w' -> W
    | 'L' | 'l' -> L
    | 'H' | 'h' -> H
    | '_' -> Don't_care
    | _ as char ->
      raise_s [%message "[Std_logic.of_char_exn] got invalid char" (char : char)]
  ;;

  let to_x : t -> Bits_list.X.t = function
    | L0 -> F
    | L1 -> T
    | U | X | Z | W | L | H | Don't_care -> X
  ;;

  let of_x : Bits_list.X.t -> t = function
    | F -> L0
    | T -> L1
    | X -> X
  ;;

  let gnd = L0
  let vdd = L1
  let as_x f a b = of_x (f (to_x a) (to_x b))
  let ( &: ) = as_x Bits_list.X.( &: )
  let ( |: ) = as_x Bits_list.X.( |: )
  let ( ^: ) = as_x Bits_list.X.( ^: )
  let ( ~: ) a = of_x (Bits_list.X.( ~: ) (to_x a))
end

module Four_state = struct
  type t =
    | X
    | Z
    | L0
    | L1
  [@@deriving compare, enumerate, sexp, variants]

  let optimise_muxs = false
  let constant_only = true
  let equal = [%compare.equal: t]

  let to_char = function
    | X -> 'x'
    | Z -> 'z'
    | L0 -> '0'
    | L1 -> '1'
  ;;

  let of_char = function
    | 'X' -> X
    | 'Z' -> Z
    | '0' -> L0
    | '1' -> L1
    | _ -> raise_s [%message "invalid char"]
  ;;

  let sexp_of_t t = [%sexp (to_char t : char)]
  let to_int = Variants.to_rank

  let of_char_exn = function
    | 'X' | 'x' -> X
    | 'Z' | 'z' -> Z
    | '0' -> L0
    | '1' -> L1
    | _ as char ->
      raise_s [%message "[Std_logic.of_char_exn] got invalid char" (char : char)]
  ;;

  let to_x : t -> Bits_list.X.t = function
    | L0 -> F
    | L1 -> T
    | Z | X -> X
  ;;

  let of_x : Bits_list.X.t -> t = function
    | F -> L0
    | T -> L1
    | X -> X
  ;;

  let gnd = L0
  let vdd = L1
  let as_x f a b = of_x (f (to_x a) (to_x b))
  let ( &: ) = as_x Bits_list.X.( &: )
  let ( |: ) = as_x Bits_list.X.( |: )
  let ( ^: ) = as_x Bits_list.X.( ^: )
  let ( ~: ) a = of_x (Bits_list.X.( ~: ) (to_x a))
end

module Std_logic_vector = Bits_list.Make (Std_logic)
module Bit_vector = Bits_list.Int_comb
module Four_state_vector = Bits_list.Make (Four_state)
