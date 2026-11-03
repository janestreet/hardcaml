(** A 4-state (0, 1, X, Z) logic value.

    A value is a pair of equal-width [Bits.t] planes:

    - [bits]: the logical value of each bit
    - [mask]: per-bit impurity. Where [mask=0], [bits] holds the real 0/1 value. Where
      [mask=1], the bit is impure: [bits=1] means X and [bits=0] means Z. *)

open! Core0

type t [@@deriving bin_io]

val compare : t -> t -> int
val ( = ) : t -> t -> bool

(** Errors if the planes have different widths. *)
val create : bits:Bits.t -> mask:Bits.t -> t Or_error.t

val create_exn : bits:Bits.t -> mask:Bits.t -> t
val bits : t -> Bits.t
val mask : t -> Bits.t
val of_bits : Bits.t -> t
val to_bits_exn : t -> Bits.t

module Impurity : sig
  (** How a value reads when it cannot be shown as a number. Any X value takes precedence
      over Z values. *)
  type t =
    | Pure
    | X
    | Z
  [@@deriving equal, sexp_of]
end

val impurity : t -> Impurity.t
val is_pure : t -> bool

(** All bits Z. *)
val high_impedance : int -> t

(** All bits X. *)
val don't_care : int -> t

(** All bits X, which is what an operation with an undefined result evaluates to (for
    example a memory read at an X address). Same as [don't_care]. *)
val undefined : int -> t

(** The [(bits, mask)] characters which encode a single 4-state bit. *)
val plane_chars : Logic.Four_state.t -> char * char

(** X(/Z)'s in argument position will tend to produce X's in the result.

    - And, Or - calculated per bit
    - Arithmetic and comparisons - any X(/Z) in arguments results in X result
    - Mux - X(/Z) in select results in X results *)
module Pessimistic : Comb.S with type t = t

(** A mixture of X(/Z)'s and values can imply a non-X result ie [0&X = 0].

    - And - calculated per bit. A 0 on one side implies a 0 result.
    - Or - calculated per bit. A 1 on one side implies a 1 result.
    - Eq - detect if any bit position computes inequality which implies a 0 result.
    - Mux - same as pessimistic (there /are/ more optimistic rules, but they differ by
      simulators and rely on how default cases are generated/reached - it's hard to do
      something general here without adding a load more logic types). *)
module Optimistic : Comb.S with type t = t

(** Default pessimistic logic. *)
include Comb.S with type t := t
(** @inline *)
