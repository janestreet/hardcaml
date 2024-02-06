open Base

module type Bits = sig
  type t = private Constant.t [@@deriving compare, sexp_of]

  include Comb.S with type t := t
  include Comparator.S with type t := t

  (** The number of bytes used to represent the data in [t]. This excludes
      any bytes used to represent any associated metadata.
  *)
  val number_of_data_bytes : t -> int

  (** Get the i-th 64-bit word within the underlying representation. *)
  val unsafe_get_int64 : t -> int -> int64

  (** Set the i-th 64-bit word within the underlying representation. *)
  val unsafe_set_int64 : t -> int -> int64 -> unit

  val get_int64 : t -> int -> int64
  val set_int64 : t -> int -> int64 -> unit

  module Expert : sig
    (** Access the underlying data representation. Note that this is unstable,
        and may change over time.
    *)
    val unsafe_underlying_repr : t -> bytes

    (** Offset to access actual data within in the underlying repr. *)
    val offset_for_data : int
  end

  (** [Mutable] is a mutable bits used by [Cyclesim] for efficiency. *)
  module Mutable : sig
    type bits
    type t = private bytes

    val number_of_data_bytes : t -> int
    val empty : t
    val width : t -> int
    val to_string : t -> string

    (** Create a [t] of given width, initially set to [0]. *)
    val create : int -> t

    val copy : src:t -> dst:t -> unit
    val copy_bits : src:bits -> dst:t -> unit

    (** A [Bits.Mutable.t] can be accessed as an array of 64 bit words. *)
    val num_words : t -> int

    val unsafe_get_int64 : t -> int -> int64
    val unsafe_set_int64 : t -> int -> int64 -> unit
    val get_int64 : t -> int -> int64
    val set_int64 : t -> int -> int64 -> unit
    val to_bits : t -> bits
    val of_constant : Constant.t -> t
    val to_constant : t -> Constant.t
    val vdd : t
    val gnd : t
    val wire : int -> t
    val ( -- ) : t -> string -> t
    val ( &: ) : t -> t -> t -> unit
    val ( |: ) : t -> t -> t -> unit
    val ( ^: ) : t -> t -> t -> unit
    val ( ~: ) : t -> t -> unit
    val ( +: ) : t -> t -> t -> unit
    val ( -: ) : t -> t -> t -> unit
    val ( ==: ) : t -> t -> t -> unit
    val ( <>: ) : t -> t -> t -> unit
    val ( <: ) : t -> t -> t -> unit
    val mux : t -> t -> t list -> unit
    val concat : t -> t list -> unit
    val concat_rev_array : t -> t array -> unit
    val select : t -> t -> int -> int -> unit
    val ( *: ) : t -> t -> t -> unit
    val ( *+ ) : t -> t -> t -> unit

    (** Mask the unused bits to zero. *)
    val mask : t -> unit

    module Comb : Comb.S with type t = t
  end
  with type bits := t

  (** Pretty printer. *)
  val pp : Formatter.t -> t -> unit
end
