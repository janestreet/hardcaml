(** A dynamically-sized array, similar to std::vector in C++. *)

open Base

module type Data = sig
  type t [@@deriving sexp_of, compare, equal]

  val width : t -> int
  val length : t -> int
  val get : t -> int -> Bits.t
  val create : int -> t
  val init : int -> width:int -> f:(int -> Bits.t) -> t
  val set : t -> int -> Bits.t -> unit
  val set_mutable_unsafe : t -> int -> Bits.Mutable.t -> unit
  val set_from_bytes : int -> t -> int -> Bytes.t -> int -> unit
  val non_cache_hits : t -> int
  val get_digestible_string : t -> Bytes.t * int
end
