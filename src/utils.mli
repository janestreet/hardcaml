(** Utility functions *)

open! Import

(** sign designator *)
module Signedness : sig
  type t = Signed | Unsigned [@@deriving sexp_of]
end

val platform_bits : int

(** forward composition *)
val (>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** reverse composition *)
val (<<) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b

(** Integer to hex character (0..15) *)
val int_of_hchar : char -> int

(** 0 or 1 -> '0' or '1' *)
val int_of_bchar : char -> int

(** converts a binary string to an integer *)
val int_of_bstr : string -> int

(** converts a binary string to an integer *)
val int32_of_bstr : string -> int32

(** converts a binary string to an integer *)
val int64_of_bstr : string -> int64

(** converts a binary string to an integer *)
val nativeint_of_bstr : string -> nativeint

(** converts an int to a binary string *)
val bstr_of_int : int -> int -> string
val bstr_of_int32 : int -> Int32.t -> string
val bstr_of_int64 : int -> Int64.t -> string
val bstr_of_nint : int -> Nativeint.t -> string

(** convert binary string to int bits list *)
val intbitslist_of_bstr : string -> int list

(** convert a list of bits (from type IntbitsList) to binary string *)
val bstr_of_intbitslist : int list -> string

(** Convert a hexidecimal string to an integer *)
val int_of_hstr : string -> int

(** Convert a string in hexadecimal notation to a binary string.  If the hex string is
    shorter than the required width, and the value is signed, the result is sign
    extended. *)
val bstr_of_hstr : Signedness.t -> int -> string -> string

(** convert a binary string to a hex string *)
val hstr_of_bstr : Signedness.t -> string -> string

(** binary string to array of int32 *)
val abits_int32_of_bstr : string -> int32 array

(** array of int32 to binary string *)
val bstr_of_abits_int32 : int -> int32 array -> string

(** binary string to array of int64 *)
val abits_int64_of_bstr : string -> int64 array

(** array of int64 to binary string *)
val bstr_of_abits_int64 : int -> int64 array -> string

(** binary string to array of nativeint *)
val abits_nint_of_bstr : string -> nativeint array

(** array of nativeint to binary string *)
val bstr_of_abits_nint : int -> nativeint array -> string

(** binary string to array of int *)
val abits_int_of_bstr : string -> int array

(** array of int to binary string *)
val bstr_of_abits_int : int -> int array -> string

(** converts a big_int to a binary string *)
val bstr_of_big_int : int -> Big_int.big_int -> string

(** converts a binary string to a big int *)
val big_int_of_bstr : string -> Big_int.big_int

(** binary Big_int.big_int to array of int32 *)
val abits_int32_of_big_int : int -> Big_int.big_int -> int32 array

(** array of int32 to binary Big_int.big_int *)
val big_int_of_abits_int32 : int32 array -> Big_int.big_int

(** binary Big_int.big_int to array of int32 *)
val abits_int64_of_big_int : int -> Big_int.big_int -> int64 array

(** array of int32 to binary Big_int.big_int *)
val big_int_of_abits_int64 : int64 array -> Big_int.big_int

(** binary Big_int.big_int to array of int32 *)
val abits_nint_of_big_int : int -> Big_int.big_int -> nativeint array

(** array of int32 to binary Big_int.big_int *)
val big_int_of_abits_nint : nativeint array -> Big_int.big_int

(** number of bits required to represent the given int *)
val nbits : int -> int

(** create list from [0...N] *)
val range : int -> int list

(** select elements from list; head of list first *)
val lselect : 'a list -> int -> int -> 'a list

(** get even elements of list *)
val leven : 'a list -> 'a list

(** get odd elements of list *)
val lodd : 'a list -> 'a list

(** create pairs from list *)
val pairs : 'a list -> ('a * 'a) list

(** split list on power of two boundary *)
val split_pow2 : 'a list -> 'a list * 'a list
