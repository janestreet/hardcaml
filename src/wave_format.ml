open Base

type t =
  | Binary
  | Bit
  | Bit_or of t
  | Hex
  | Unsigned_int
  | Int
  | Index of string list
  | Custom of (Bits.t -> string)
[@@deriving sexp_of]

let rec equal a b =
  match a, b with
  | Binary, Binary | Bit, Bit | Hex, Hex | Int, Int | Unsigned_int, Unsigned_int -> true
  | Bit_or a, Bit_or b -> equal a b
  | Index a, Index b -> [%compare.equal: string list] a b
  | Custom f, Custom g -> phys_equal f g
  | (Bit | Bit_or _ | Binary | Hex | Unsigned_int | Int | Index _ | Custom _), _ -> false
;;

let rec to_string t (bits : Bits.t) =
  match t with
  | Binary | Bit -> Constant.to_binary_string (bits :> Constant.t)
  | Bit_or _ when Bits.width bits = 1 -> Constant.to_binary_string (bits :> Constant.t)
  | Bit_or t -> to_string t bits
  | Hex -> Constant.to_hex_string ~signedness:Unsigned (bits :> Constant.t)
  | Unsigned_int ->
    let width = Bits.width bits in
    if width <= 62
    then Bits.to_int bits |> Int.to_string
    else Constant.to_z ~signedness:Unsigned (bits :> Constant.t) |> Zarith.Z.to_string
  | Int ->
    let width = Bits.width bits in
    if width <= 63
    then Bits.to_int bits |> Int.to_string
    else Constant.to_z ~signedness:Signed (bits :> Constant.t) |> Zarith.Z.to_string
  | Index strings ->
    (match List.nth strings (Bits.to_int bits) with
     | None -> "-"
     | Some s -> s)
  | Custom f -> f bits
;;
