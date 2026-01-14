open! Core0

(* We cannot serialize the [Custom] constructor. Instead we map it to a simple conversion
   to a bit string. *)
module Custom = struct
  module M = struct
    type t = unit [@@deriving bin_io]
  end

  module T = struct
    type t = Bits.t -> string [@@deriving sexp_of]

    let of_binable () =
      let simple_conversion t = Bits.to_string t in
      simple_conversion
    ;;

    let to_binable (_ : t) = ()

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "7f52c42b-a0bf-4241-ba12-31e009fd768d"
    ;;
  end

  include T
  include Binable.Of_binable_with_uuid (M) (T)
end

type t =
  | Binary
  | Bit
  | Bit_or of t
  | Hex
  | Unsigned_int
  | Int
  | Index of string list
  | Custom of Custom.t
  | Map of (Bits.t, string) List.Assoc.t
[@@deriving bin_io, sexp_of]

let%template[@mode local] rec equal (a @ local) (b @ local) =
  match a, b with
  | Binary, Binary | Bit, Bit | Hex, Hex | Int, Int | Unsigned_int, Unsigned_int -> true
  | Bit_or a, Bit_or b -> (equal [@mode local]) a b
  | Index a, Index b -> ([%compare.equal: string list] [@mode local]) a b
  | Custom f, Custom g -> phys_equal f g
  | Map m, Map n -> ([%equal: (Bits.t * string) list] [@mode local]) m n
  | (Bit | Bit_or _ | Binary | Hex | Unsigned_int | Int | Index _ | Custom _ | Map _), _
    -> false
;;

let%template equal = [%eta2 equal [@mode local]]

let rec to_string t =
  match t with
  | Binary | Bit ->
    (fun (bits : Bits.t) -> Constant.to_binary_string (bits :> Constant.t))
    |> Staged.stage
  | Bit_or t ->
    let to_string (bits : Bits.t) =
      if Bits.width bits = 1
      then Constant.to_binary_string (bits :> Constant.t)
      else Staged.unstage (to_string t) bits
    in
    Staged.stage to_string
  | Hex ->
    let to_string (bits : Bits.t) =
      Constant.to_hex_string ~capitalize:true ~signedness:Unsigned (bits :> Constant.t)
    in
    Staged.stage to_string
  | Unsigned_int ->
    let to_string (bits : Bits.t) =
      match Bits.to_unsigned_int bits |> Int.to_string with
      | exception _ ->
        Constant.to_bigint ~signedness:Unsigned (bits :> Constant.t) |> Bigint.to_string
      | bits -> bits
    in
    Staged.stage to_string
  | Int ->
    let to_string (bits : Bits.t) =
      match Bits.to_signed_int bits |> Int.to_string with
      | exception _ ->
        Constant.to_bigint ~signedness:Signed (bits :> Constant.t) |> Bigint.to_string
      | bits -> bits
    in
    Staged.stage to_string
  | Index strings ->
    let strings = Array.of_list strings in
    let to_string (bits : Bits.t) =
      match strings.(Bits.to_int_trunc bits) with
      | exception _ -> "-"
      | s -> s
    in
    Staged.stage to_string
  | Map map ->
    let map = Map.of_alist_exn (module Bits) map in
    let to_string (bits : Bits.t) =
      match Map.find map bits with
      | None -> "-"
      | Some s -> s
    in
    Staged.stage to_string
  | Custom f -> Staged.stage f
;;

let default = Bit_or Hex
