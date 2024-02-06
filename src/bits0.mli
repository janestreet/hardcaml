(** The underlying representations of [Constant.t] and [Bits.t].  Not exposed by the
    Hardcaml interface.

    They are represented by 2 fields - a bit [width] and [data] represented as a
    [Bytes.t].

    Abstractly, [data] is thought of as an array of 64 bit integers. Even a single bit
    vector requires 64 bits to store it. Any unused upper bits must be set to [0] - this
    is a requirement of functions over [Bits].

    It is represented with [Bytes.t] because that is a more efficient memory layout than
    an ocaml array of [Int64]s.
*)
open Base

type t = private Bytes.t [@@deriving compare, sexp, bin_io]

module Comparable : Comparable.S with type t := t

val bits_per_word : int
val log_bits_per_word : int
val shift_bits_to_bytes : int
val shift_bytes_to_words : int
val width_mask : int
val words_of_width : int -> int
val bytes_of_width : int -> int
val offset_for_data : int

(* The empty constant. Contains no bits. *)
val empty : t

(** [create n] create a [n] bit constant, initialized to hold all [0]s. *)
val create : int -> t

(** The number of 64 bit words used to represent the constant *)
val words : t -> int

(** The number of bytes used to represent the constant. *)
val number_of_data_bytes : t -> int

(** Bit width of constant *)
val width : t -> int

(** Create a constant of width [width] from a byte at a time. *)
val init_byte : width:int -> f:(int -> char) -> t

(** Similar to [init_byte], but constructs a 64-bit word at a time. *)
val init_int64 : width:int -> f:(int -> int64) -> t

(** Get the n-th byte of the constant. *)
val unsafe_get_byte : t -> int -> char

(** Get the n-th 64-bit word of the constant. [unsafe_get_int64 t pos] accesses
    the data at the [pos * 8]-th byte of t.
*)
val unsafe_get_int64 : t -> int -> int64

(** Similar to [unsafe_get_int64], but for writing instead of reading. *)
val unsafe_set_int64 : t -> int -> int64 -> unit

val get_int64 : t -> int -> int64
val set_int64 : t -> int -> int64 -> unit

(** Get the n-th 32-bit word of the constant. [unsafe_get_int32 t pos] accesses
    the data at the [pos * 4]-th byte of t.
*)
val unsafe_get_int32 : t -> int -> int32

val unsafe_set_int32 : t -> int -> int32 -> unit

(** Maskout the unused bits to zeros *)
val mask : t -> unit

val blit_data : t -> t -> unit
