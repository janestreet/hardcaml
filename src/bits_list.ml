open Base

(* Bits API built using lists to represent vectors. Allows any bit precision and is
   simple, but slow. *)

module type T = sig
  type t [@@deriving equal, sexp]

  val constant_only : bool
  val optimise_muxs : bool
  val vdd : t
  val gnd : t
  val ( &: ) : t -> t -> t
  val ( |: ) : t -> t -> t
  val ( ^: ) : t -> t -> t
  val ( ~: ) : t -> t
  val to_char : t -> char
  val of_char : char -> t
end

module type Comb = sig
  include Comb.S

  val t_of_sexp : Sexp.t -> t
end

module Make_gates (T : T) = struct
  type t = T.t list [@@deriving equal, sexp]

  let empty = []
  let is_empty = List.is_empty
  let width x = List.length x

  let of_constant x =
    Constant.to_bit_list x
    |> List.map ~f:(function
         | 0 -> T.gnd
         | 1 -> T.vdd
         | _ -> raise_s [%message "invalid constant"])
  ;;

  let to_constant c =
    List.map c ~f:(fun c ->
      if T.equal c T.gnd
      then 0
      else if T.equal c T.vdd
      then 1
      else raise_s [%message "invalid constant"])
    |> Constant.of_bit_list
  ;;

  let concat_msb l = List.concat l

  let select s h l =
    let rec sel b i =
      match b with
      | [] -> []
      | hd :: tl ->
        if i > h then [] else if i >= l then hd :: sel tl (i + 1) else sel tl (i + 1)
    in
    List.rev (sel (List.rev s) 0)
  ;;

  let ( &: ) = List.map2_exn ~f:T.( &: )
  let ( |: ) = List.map2_exn ~f:T.( |: )
  let ( ^: ) = List.map2_exn ~f:T.( ^: )
  let ( ~: ) = List.map ~f:T.( ~: )

  let to_bstr b =
    let rec f b =
      match b with
      | [] -> []
      | h :: t -> T.to_char h :: f t
    in
    String.of_char_list (f b)
  ;;

  let to_bstr = to_bstr
  let to_string t = Sexp.to_string_hum (sexp_of_t t)
  let ( -- ) a _ = a

  let sexp_of_t =
    if T.constant_only
    then (
      let sexp_of_t t = [%sexp (to_bstr t : string)] in
      sexp_of_t)
    else sexp_of_t
  ;;

  let t_of_sexp =
    if T.constant_only
    then (
      let t_of_sexp t =
        let s = String.t_of_sexp t in
        String.to_list s |> List.map ~f:T.of_char
      in
      t_of_sexp)
    else t_of_sexp
  ;;

  let to_string =
    if T.constant_only
    then (
      let to_string t = Sexp.to_string_hum (sexp_of_t t) in
      to_string)
    else to_string
  ;;
end

module Int = struct
  type t = int [@@deriving compare, equal, sexp]

  let optimise_muxs = true
  let constant_only = true
  let vdd = 1
  let gnd = 0
  let ( &: ) a b = a land b
  let ( |: ) a b = a lor b
  let ( ^: ) a b = a lxor b
  let ( ~: ) a = if a = 1 then 0 else 1

  let to_char = function
    | 1 -> '1'
    | 0 -> '0'
    | _ -> raise_s [%message "invalid char"]
  ;;

  let of_char = function
    | '1' -> 1
    | '0' -> 0
    | _ -> raise_s [%message "invalid char"]
  ;;
end

module Bool = struct
  type t = bool [@@deriving compare, equal, sexp]

  let optimise_muxs = true
  let constant_only = true
  let vdd = true
  let gnd = false
  let ( &: ) a b = a && b
  let ( |: ) a b = a || b
  let ( ^: ) a b = Bool.(a <> b)
  let ( ~: ) a = not a

  let to_char = function
    | true -> '1'
    | false -> '0'
  ;;

  let of_char = function
    | '1' -> true
    | '0' -> false
    | _ -> raise_s [%message "invalid char"]
  ;;
end

module X = struct
  type t =
    | F
    | T
    | X
  [@@deriving compare, equal, sexp]

  let optimise_muxs = false
  let constant_only = true
  let vdd = T
  let gnd = F

  (* In the following, we assume the result of an operation with an X argument, is X.
     But, for example, [1 |: X] we know can only be [1]. Perhaps a [U] (unknown)
     constructor should work like that? Should check the resolution function rules of
     VHDL to be sure. *)
  let ( &: ) a b =
    match a, b with
    | X, _ | _, X -> X
    | T, T -> T
    | T, F | F, T | F, F -> F
  ;;

  let ( |: ) a b =
    match a, b with
    | X, _ | _, X -> X
    | F, F -> F
    | T, F | F, T | T, T -> T
  ;;

  let ( ^: ) a b =
    match a, b with
    | X, _ | _, X -> X
    | F, F | T, T -> F
    | T, F | F, T -> T
  ;;

  let ( ~: ) a =
    match a with
    | X -> X
    | F -> T
    | T -> F
  ;;

  let to_char = function
    | T -> '1'
    | F -> '0'
    | X -> 'x'
  ;;

  let of_char = function
    | '1' -> T
    | '0' -> F
    | 'x' -> X
    | _ -> raise_s [%message "invalid char"]
  ;;
end

module Make (T : T) = struct
  module Gates = Make_gates (T)
  module Primitives = Comb.Make_primitives (Gates)
  include Comb.Make (Primitives)

  let t_of_sexp = Gates.t_of_sexp

  let mux =
    if T.optimise_muxs && T.constant_only
    then (
      let mux sel vals =
        let len = List.length vals in
        let idx = to_constant sel |> Constant.to_int64 |> Int64.to_int_trunc in
        List.nth_exn vals (if idx >= len then len - 1 else idx)
      in
      mux)
    else mux
  ;;
end

module Int_comb = Make (Int)
module Bool_comb = Make (Bool)

module X_comb = struct
  include Make (X)

  let of_bit_string s =
    String.to_list s
    |> List.filter_map ~f:(function
         | '0' -> Some X.F
         | '1' -> Some X.T
         | 'x' | 'X' -> Some X.X
         | '_' -> None
         | _ -> raise_s [%message "invalid character in bit string"])
  ;;

  let of_string s =
    try of_bit_string s with
    | _ -> of_string s
  ;;
end
