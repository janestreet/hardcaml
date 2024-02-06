(** Functions for working with bit vectors packed into a single [Bytes.t].

    The size and offset of data is passed to the functions rather than embedded like with
    [Bits.t].
*)

open Base

type t = Bytes.t

type op2_width =
  t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit

type op2_size =
  t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> size_in_words:int
  -> unit

type op2_mul =
  t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits_a:int
  -> width_in_bits_b:int
  -> unit

val add : op2_width
val sub : op2_width
val mulu : op2_mul
val muls : op2_mul
val and_ : op2_size
val or_ : op2_size
val xor : op2_size
val eq : op2_size
val lt : op2_size
val not' : t -> dst_address:int -> src_address:int -> width_in_bits:int -> unit

module Cat_src : sig
  type t =
    { address : int
    ; width : int
    }
end

val cat : t -> dst_address:int -> Cat_src.t list -> width_in_bits:int -> unit
val select : t -> dst_address:int -> src_address:int -> high:int -> low:int -> unit

module Cat_src_array : sig
  type t

  val create : Cat_src.t list -> t array
end

val cat_array : t -> dst_address:int -> Cat_src_array.t array -> unit

val mux
  :  t
  -> dst_address:int
  -> select_address:int
  -> cases:int array
  -> size_in_words:int
  -> unit
