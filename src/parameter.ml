open Base

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

module Std_logic_vector = Bits_list.Make (Std_logic)
module Bit_vector = Bits_list.Int_comb

module Value = struct
  type t =
    | Bit of bool
    | Bit_vector of Bit_vector.t
    | Bool of bool
    | Int of int
    | Real of float
    | Std_logic of Std_logic.t
    | Std_logic_vector of Std_logic_vector.t
    | Std_ulogic of Std_logic.t
    | Std_ulogic_vector of Std_logic_vector.t
    | String of string
  [@@deriving equal, sexp, variants]
end

type t =
  { name : Parameter_name.t
  ; value : Value.t
  }
[@@deriving equal, sexp_of]

let sexp_of_t { name; value } =
  [%message "" ~_:(name : Parameter_name.t) ~_:(value : Value.t)]
;;

let create ~name ~value = { name = name |> Parameter_name.of_string; value }

let find_name ts name =
  List.find_map ts ~f:(fun t ->
    if Parameter_name.equal t.name name then Some t.value else None)
;;

let find_name_exn ts name =
  match find_name ts name with
  | Some x -> x
  | None ->
    raise_s
      [%message
        "couldn't find parameter" (name : Parameter_name.t) ~parameters:(ts : t list)]
;;

let is_subset ts1 ts2 =
  List.for_all ts1 ~f:(fun t1 ->
    match find_name ts2 t1.name with
    | Some v2 -> Value.equal t1.value v2
    | None -> false)
;;

let sort_by_name ts =
  List.sort ts ~compare:(fun t1 t2 -> Parameter_name.compare t1.name t2.name)
;;
