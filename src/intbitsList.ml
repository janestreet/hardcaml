open! Import

(* Bits API built using lists on ints.  Allows any bit precision and is simple, but
   slow. *)

module Gates = struct
  (* msb first *)
  type t = int list [@@deriving compare]

  let equal = [%compare.equal: t]
  let empty = []
  let is_empty = List.is_empty
  let width x = List.length x
  let of_constant = Constant.to_bit_list
  let to_constant = Constant.of_bit_list
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

  let ( &: ) = List.map2_exn ~f:( land )
  let ( |: ) = List.map2_exn ~f:( lor )
  let ( ^: ) = List.map2_exn ~f:( lxor )
  let ( ~: ) = List.map ~f:(fun x -> if x = 1 then 0 else 1)

  let rec to_string b =
    match b with
    | [] -> ""
    | h :: t -> (if h = 1 then "1" else "0") ^ to_string t
  ;;

  let to_bstr = to_string
  let sexp_of_t s = [%sexp (to_bstr s : string)]
  let ( -- ) a _ = a
end

module Primitives = struct
  include Comb.Make_primitives (Gates)

  (* About 30% faster than the generic implementation, and common enough to care. *)
  let mux sel vals =
    let len = List.length vals in
    let idx = to_constant sel |> Constant.to_int64 |> Int64.to_int_trunc in
    List.nth_exn vals (if idx >= len then len - 1 else idx)
  ;;
end

include Comb.Make (Primitives)
