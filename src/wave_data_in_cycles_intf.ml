(** A dynamically-sized array, similar to std::vector in C++. *)

open! Core0

module type Wave_data_in_cycles = sig
  type t [@@deriving sexp_of, compare ~localize, equal ~localize, bin_io]

  val width : t -> int
  val length : t -> int
  val num_events : t -> int
  val find_insertion_index : t -> int -> int option
  val get_time_at_index : t -> int -> int

  val%template get_data_at_index : t @ m -> int -> Bits.t [@@mode m = (global, local)]
  val%template get : t @ m -> int -> Bits.t [@@mode m = (global, local)]

  val create : int -> t
  val init : int -> width:int -> f:(int -> Bits.t) -> t
  val set : t -> int -> Bits.t -> unit
  val set_mutable_unsafe : t -> int -> Bits.Mutable.t -> unit
  val set_from_bytes : int -> t -> int -> Bytes.t -> int -> unit
  val non_cache_hits : t -> int
  val get_digestible_string : t -> Bytes.t * int
end
