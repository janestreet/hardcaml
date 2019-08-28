(** The underlying representations of [Constant.t] and [Bits.t].  Not exposed by the
    Hardcaml interface. *)
open! Import

type t = private
  { width : int
  ; data : Bytes.t
  }
[@@deriving compare, sexp_of]

module Comparable : Comparable.S with type t := t

val bits_per_word : int
val log_bits_per_word : int
val shift_bits_to_bytes : int
val shift_bytes_to_words : int
val width_mask : int
val words_of_width : int -> int

(* The empty constant. Contains no bits. *)
val empty : t

(** [create n] create a [n] bit constant, initialized to hold all [0]s. *)
val create : int -> t

(** [create_bytes width] returns a [Bytes.t] large enough to hold [width] bits and
    rouneded up appropriately. *)
val create_bytes : int -> Bytes.t

(** Construct a [t] with the given [width] and [Bytes.t] data. *)
val init : width:int -> data:Bytes.t -> t

(** The number of 64 bit words used to represent the constant *)
val words : t -> int

(** Bit width of constant *)
val width : t -> int
