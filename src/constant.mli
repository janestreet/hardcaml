(** Representation of variable width Constants and conversion to/from OCaml types. *)

open Base

type t = Bits0.t [@@deriving compare, sexp, bin_io]

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

(** Convert to an arbitrary precision integer. *)
val to_z : signedness:Signedness.t -> t -> Zarith.Z.t

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

(** Create from an octal encoded string. *)
val of_octal_string : signedness:Signedness.t -> width:int -> string -> t

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

(** Convert to from constants from raw bit patterns stored in strings and bytes. Data is
    copied and resized as appropriate.
*)
module Raw : sig
  (** Convert from a byte buffer.  The copied data is padded as required. *)
  val of_bytes : Bytes.t -> width:int -> t

  (** Convert from a string buffer.  The copied data is padded as required. *)
  val of_string : String.t -> width:int -> t

  (** Convert to a string buffer. The output buffer length is rounded to a multiple of 8
      bits . *)
  val to_string : t -> String.t

  val to_bytes : t -> Bytes.t
end

module Expert : sig
  val offset_for_data : int
end
