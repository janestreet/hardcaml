(** A [Bits.t] is an immutable finite sequence of bits.  It can also be viewed as an
    non-negative integer. *)

open! Import

module type Bits = sig

  type t [@@deriving compare]

  include Comb.S       with type t := t
  include Comparator.S with type t := t

  (** [Mutable] is a mutable bits used by [Cyclesim] for efficiency. *)
  module Mutable : sig
    type bits

    type t

    val empty : t
    val width : t -> int

    val to_string : t -> string
    val to_int : t -> int
    val to_bstr : t -> string

    (** Create a [t] of given width, initially set to [0]. *)
    val create : int -> t

    val copy      : src:t    -> dst:t -> unit
    val copy_bits : src:bits -> dst:t -> unit

    (** A [Bits.Mutable.t] contains an array of 62-bit words.  [num_words], [get_words],
        and [set_words] are like Array [length], [get], and [set]. *)
    val num_words  : t -> int
    val get_word : t -> int -> int
    val set_word : t -> int -> int -> unit

    val to_bits : t -> bits

    val const : string -> t
    val vdd : t
    val gnd : t

    val wire : int -> t
    val (--) : t -> string -> t

    val (&:) : t -> t -> t -> unit
    val (|:) : t -> t -> t -> unit
    val (^:) : t -> t -> t -> unit

    val (~:) : t -> t -> unit

    val (+:) : t -> t -> t -> unit
    val (-:) : t -> t -> t -> unit

    val (==:) : t -> t -> t -> unit
    val (<>:) : t -> t -> t -> unit
    val (<:) : t -> t -> t -> unit

    val mux : t -> t -> t list -> unit

    val concat : t -> t list -> unit
    val select : t -> t -> int -> int -> unit

    val ( *: ) : t -> t -> t -> unit
    val ( *+ ) : t -> t -> t -> unit

    module Comb : Comb.S with type t = t
  end with type bits := t
end
