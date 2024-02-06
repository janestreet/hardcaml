(** Functions for accessing combinational nodes, registers and memories in a simulation.

    Combinational nodes may be read. Registers and memories may also be written.

    Though data is represented as a [Bytes.t] we assume throughout that we are really
    working with arrays of [Int64.t]s.
*)

open Base

module type READ = sig
  type t

  val data : t -> Bytes.t

  (** Offset within [data]. *)
  val byte_address : t -> int

  val width_in_bits : t -> int
  val size_in_words : t -> int

  (** Create node from a [Bits.Mutable.t]. *)
  val create_from_bits_mutable : Bits.Mutable.t -> t

  (** Create from from within [data] at [byte_address]. *)
  val create_from_signal : byte_address:int -> data:Bytes.t -> Signal.t -> t

  val unsafe_get64 : t -> int -> Int64.t
  val get64 : t -> int -> Int64.t
  val to_bits_mutable : t -> Bits.Mutable.t -> unit
  val to_bits : t -> Bits.t
  val to_int : t -> int
end

module type WRITE = sig
  type t

  val unsafe_set64 : t -> int -> Int64.t -> unit
  val set64 : t -> int -> Int64.t -> unit
  val of_bits_mutable : t -> Bits.Mutable.t -> unit
  val of_bits : t -> Bits.t -> unit
  val of_int : t -> int -> unit
end

module type Cyclesim_lookup = sig
  module Node : READ

  module Reg : sig
    type t

    include READ with type t := t
    include WRITE with type t := t

    (** Convert to read-only [Node.t]. *)
    val to_node : t -> Node.t

    (** Create a reg which can only be read. Raises at runtime if written. *)
    val read_only_of_node : Node.t -> t
  end

  (** Read and write access for memories. They have much the same API as [Node]s. and
      [Reg]s except for an extra [address] argument. *)
  module Memory : sig
    type t

    val width_in_bits : t -> int
    val size_in_words : t -> int
    val memory_size : t -> int
    val create_from_bits_mutable_array : Bits.Mutable.t array -> t
    val create_from_read_only_bits_array : Bits.t array -> t
    val create_from_signal : byte_address:int -> data:Bytes.t -> Signal.t -> t
    val unsafe_set64 : t -> address:int -> int -> Int64.t -> unit
    val unsafe_get64 : t -> address:int -> int -> Int64.t
    val set64 : t -> address:int -> int -> Int64.t -> unit
    val get64 : t -> address:int -> int -> Int64.t
    val of_bits_mutable : t -> address:int -> Bits.Mutable.t -> unit
    val to_bits_mutable : t -> address:int -> Bits.Mutable.t -> unit
    val of_bits : t -> address:int -> Bits.t -> unit
    val to_bits : t -> address:int -> Bits.t
    val to_int : t -> address:int -> int
    val of_int : t -> address:int -> int -> unit
    val read_all : t -> Bits.t array
  end
end
