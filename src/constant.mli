(** Representation of variable width Constants and conversion to/from OCaml types. *)

open! Import

type t = Bits0.t [@@deriving compare, sexp_of]

module Signedness : sig
  type t =
    | Signed
    | Unsigned
  [@@deriving sexp_of]
end

(* The empty constant. Contains no bits. *)
val empty : t

(** [create n] create a [n] bit constant, initialized to hold all [0]s. *)
val create : int -> t

(** Bit width of constant *)
val width : t -> int

(** Convert a constant to a string of ['1'] and ['0'] chars. *)
val to_binary_string : t -> string

(** Same as [to_binary_string] but adds ['_']s every chars *)
val to_binary_string_hum : t -> string

(** Convert constant to an [int].  Bits above [Int.num_bits] are dropped. *)
val to_int : t -> int

(** Convert constant to an [int32].  Bits above [Int32.num_bits] are dropped. *)
val to_int32 : t -> int32

(** Convert constant to an [int64].  Bits above [Int64.num_bits] are dropped. *)
val to_int64 : t -> int64

(** Convert to array of int64s *)
val to_int64_array : t -> int64 array

(** Convert to an unsigned arbitrary precision integer. *)
val to_z : t -> Zarith.Z.t

(** Convert to a hex encoded string. *)
val to_hex_string : signedness:Signedness.t -> t -> string

(** Convert a string containing ['1'] and ['0'] characters to a constant. Width is
    inferred from the strings length. *)
val of_binary_string : string -> t

(** Same as [of_binary_string] but allow ['_'] chars *)
val of_binary_string_hum : string -> t

(** Create a constant from the given [int] value. *)
val of_int : width:int -> int -> t

(** Create a constant from the given [int32] value *)
val of_int32 : width:int -> int32 -> t

(** Create a constant from the given [int64] value *)
val of_int64 : width:int -> int64 -> t

(** Create from an array of [int64]s *)
val of_int64_array : width:int -> int64 array -> t

(** Convert from an arbitrary precision integer. *)
val of_z : width:int -> Zarith.Z.t -> t

(** Create from a hex encoded string. *)
val of_hex_string : signedness:Signedness.t -> width:int -> string -> t

(** Utility conversion functions to/from hex chars *)
val int_of_hex_char : char -> int

val hex_char_of_int : int -> char

module type Bit = sig
  type t

  val vdd : t
  val gnd : t
  val equal : t -> t -> bool
end

(** Create constant conversion functions from lists of some inner [Bit.t] type. *)
module Make_bit_list (Bit : Bit) : sig
  val to_constant : Bit.t list -> t
  val of_constant : t -> Bit.t list
end

val of_bit_list : int list -> t
val to_bit_list : t -> int list
val pp : Formatter.t -> t -> unit

(** {2 Unsafe Operations} *)

(** Return the underlying [Bytes.t] representation of the constant. We label as unsafe as
    the output bytes are not copied. Mutating them should be done with care. *)
val unsafe_to_bytes : t -> Bytes.t

(** Construct a constant from the given [Bytes.t]. We label as unsafe as the input bytes
    are not copied. Mutating them should be done with care.

    The length of the given [Bytes.t] must be rounded up to 64 bits and be the correct
    size of fit [width] bits or an exception is raised. *)
val unsafe_of_bytes : width:int -> Bytes.t -> t
