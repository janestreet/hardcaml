open! Import

module type Bits = sig
  type t [@@deriving compare, sexp_of]

  include Comb.S with type t := t
  include Comparator.S with type t := t

  module Unsafe : sig
    val data : t -> bytes
  end

  (** [Mutable] is a mutable bits used by [Cyclesim] for efficiency. *)
  module Mutable : sig
    type bits
    type t

    val empty : t
    val width : t -> int
    val to_string : t -> string

    (** Create a [t] of given width, initially set to [0]. *)
    val create : int -> t

    val copy : src:t -> dst:t -> unit
    val copy_bits : src:bits -> dst:t -> unit

    (** A [Bits.Mutable.t] can be accessed as an array of 64 bit words. *)
    val num_words : t -> int

    val get_word : t -> int -> int64
    val set_word : t -> int -> int64 -> unit
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
    val select : t -> t -> int -> int -> unit
    val ( *: ) : t -> t -> t -> unit
    val ( *+ ) : t -> t -> t -> unit

    module Comb : Comb.S with type t = t
  end
  with type bits := t

  (** Pretty printer. *)
  val pp : Formatter.t -> t -> unit
end
