(** Sorting networks.

    A sorting network arranges a fixed configuration of compare-and-swaps to sort data.
    The static nature of the network means they can be easily implemented in hardware.

    This module implements [Bitonic_sort] and [Odd_even_merge_sort] networks.

    The network construction abstracts over the actual data to be sorted and the
    compare-and-swap implementation, allowing various configurations:

    - sort lists of integers
    - [Bits] or [Signals] combinatorial networks
    - pipelined
    - configure the sort's direction (invert the compare operation)
    - sort arbirary data according to some key

    An excellent resource explaining the complexity and correctness of these algorithms
    is:

    http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/networks/indexen.htm

    The algorithms are analysed using the 0-1 principle which states that a network
    that sorts any sequence of 0s and 1s also sorts arbitrary data. *)

module Config : sig
  (** The sorting algorithms require the input list's length to be a power of 2. *)
  type t =
    | Bitonic_sort
    | Odd_even_merge_sort
  [@@deriving enumerate, sexp_of]
end

module Min_or_max : sig
  type t = Min | Max [@@deriving sexp_of]
end

module Min_max : sig
  type 'a t = { min : 'a; max : 'a } [@@deriving sexp_of]

  val get : 'a t -> Min_or_max.t -> 'a
end

type 'a compare_and_swap = ('a -> 'a -> 'a Min_max.t)

val create : Config.t -> 'a compare_and_swap -> 'a list -> 'a list
