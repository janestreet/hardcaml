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

let default = Bit_or Hex

let rec equal_internal (a @ local) (b @ local) =
  match a, b with
  | Binary, Binary | Bit, Bit | Hex, Hex | Int, Int | Unsigned_int, Unsigned_int -> true
  | Bit_or a, Bit_or b -> equal_internal a b
  | Index a, Index b -> ([%compare.equal: string list] [@mode local]) a b
  | Custom f, Custom g -> phys_equal f g
  | Map m, Map n -> ([%equal: (Bits.t * string) list] [@mode local]) m n
  | (Bit | Bit_or _ | Binary | Hex | Unsigned_int | Int | Index _ | Custom _ | Map _), _
    -> false
;;

let%template[@mode __ = (local, global)] equal = [%eta2 equal_internal]
