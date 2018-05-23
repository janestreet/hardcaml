(** A fixed-point number, i.e. a number between [(- 2**(width-1) / 2**fractional_width]
    and [(2**(width-1) - 1) / 2**fractional_width] that is evenly divisible by
    [2**(-fractional_width)].  It is represented as an integer between [-2**(width-1)] and
    [2**(width-1)]. *)

open! Import

module type Spec = sig
  (** overall width *)
  val width : int
  val fractional_width : int
end

module type Fixnum = sig
  module type Spec = Spec

  module Make (Spec : Spec) : sig
    type t

    val width : int

    val of_float : float -> t
    val to_float : t -> float

    (** [const] is used with [Bits.constb] and [Signal.constb]. *)
    val constb : t -> string

    val of_bits : Bits.t -> t
    val to_bits : t -> Bits.t

    val pow2 : int -> t

    (** [to_scaled_int t] = [t * (2**fractional_width)]. *)
    val to_scaled_int : t -> int

    val bits_constf : float -> Bits.t

    val signal_mul : Signal.t -> Signal.t -> Signal.t

    val signal_constf : float -> Signal.t
  end
end
