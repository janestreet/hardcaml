module Unstable = struct
  open! Base

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
  end

  module Std_logic_vector = struct
    type t = Std_logic.t list [@@deriving compare, sexp]
  end

  module Bit_vector = struct
    type t = bool list [@@deriving compare, sexp]
  end

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
    [@@deriving compare, sexp, variants]
  end

  module T = struct
    type t =
      { name : Parameter_name.t
      ; value : Value.t
      }
    [@@deriving compare, sexp]
  end

  include T
end

open! Import

module Std_logic = struct
  module Unstable = Unstable.Std_logic
  include Unstable

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
end

module Std_logic_vector = struct
  module Unstable = Unstable.Std_logic_vector
  include Unstable

  let equal = [%compare.equal: t]
  let to_string v = v |> List.map ~f:Std_logic.to_char |> String.of_char_list
  let of_string s = s |> String.to_list |> List.map ~f:Std_logic.of_char_exn
  let sexp_of_t t = [%sexp (to_string t : string)]
  let create x = x
  let width x = List.length x
  let of_bits b = Bits.to_bstr b |> of_string
end

module Bit_vector = struct
  module Unstable = Unstable.Bit_vector
  include Unstable

  let equal = [%compare.equal: t]

  let to_string v =
    v
    |> List.map ~f:(function
      | true -> '1'
      | false -> '0')
    |> String.of_char_list
  ;;

  let of_string s =
    s
    |> String.to_list
    |> List.map ~f:(function
      | '0' -> false
      | '1' -> true
      | _ as char ->
        raise_s [%message "[Bit_vector.of_string] got invalid char" (char : char)])
  ;;

  let sexp_of_t t = [%sexp (to_string t : string)]
  let create x = x
  let width x = List.length x
  let of_bits b = Bits.to_bstr b |> of_string
end

module Value = struct
  module Unstable = Unstable.Value
  include Unstable

  let equal = [%compare.equal: t]

  let sexp_of_t = function
    | Bit b -> [%sexp (b : bool)]
    | Bit_vector v -> [%sexp (v : Bit_vector.t)]
    | Bool b -> [%sexp (b : bool)]
    | Int i -> [%sexp (i : int)]
    | Real f -> [%sexp (f : float)]
    | Std_logic x -> [%sexp (x : Std_logic.t)]
    | Std_logic_vector x -> [%sexp (x : Std_logic_vector.t)]
    | Std_ulogic x -> [%sexp (x : Std_logic.t)]
    | Std_ulogic_vector x -> [%sexp (x : Std_logic_vector.t)]
    | String s -> [%sexp (s : string)]
  ;;
end

include Unstable.T

let sexp_of_t { name; value } =
  [%message "" ~_:(name : Parameter_name.t) ~_:(value : Value.t)]
;;

let equal = [%compare.equal: t]
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
