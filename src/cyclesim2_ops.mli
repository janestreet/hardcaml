(** Runtime functions for the simulator. Implementations are selected based on operand and
    result sizes. *)

open Base

val get64 : Bytes.t -> int -> Int64.t
val set64 : Bytes.t -> int -> Int64.t -> unit

val add
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  -> unit

val sub
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  -> unit

val mulu
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits_a:int
  -> width_in_bits_b:int
  -> unit
  -> unit

val muls
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits_a:int
  -> width_in_bits_b:int
  -> unit
  -> unit

val and_
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  -> unit

val or_
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  -> unit

val xor
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  -> unit

val eq
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  -> unit

val lt
  :  Bytes.t
  -> dst_address:int
  -> src_address_a:int
  -> src_address_b:int
  -> width_in_bits:int
  -> unit
  -> unit

val not_
  :  Bytes.t
  -> dst_address:int
  -> src_address:int
  -> width_in_bits:int
  -> unit
  -> unit

val mux
  :  Bytes.t
  -> dst_address:int
  -> select_address:int
  -> select_width:int
  -> cases:int array
  -> size_in_words:int
  -> unit
  -> unit

val cat
  :  Bytes.t
  -> dst_address:int
  -> Bits_packed.Cat_src.t list
  -> width_in_bits:int
  -> unit
  -> unit

val select
  :  Bytes.t
  -> dst_address:int
  -> src_address:int
  -> high:int
  -> low:int
  -> unit
  -> unit

val mem_read
  :  Bytes.t
  -> dst_address:int
  -> read_address:int
  -> memory_address:int
  -> memory_size:int
  -> size_in_words:int
  -> unit
  -> unit

type clear =
  { clear : int
  ; clear_value : int
  ; level : int
  }

val reg
  :  Bytes.t
  -> clear:clear option
  -> enable:int option
  -> dst_address:int
  -> src_address:int
  -> size_in_words:int
  -> unit
  -> unit

val mem_write_port
  :  Bytes.t
  -> size:int
  -> memory_address:int
  -> write_enable:int
  -> write_address:int
  -> write_data:int
  -> size_in_words:int
  -> unit
  -> unit
