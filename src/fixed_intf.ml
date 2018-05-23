(** Fixed point arithmetic.

    {[
      (* select bits type *)
      module F = Fixed.Make (Bits)
      (* select Signed/Unsigned fixed type, and overflow and rounding mode. *)
      module S = F.Signed.Make (struct
          let round = F.Signed.Round.tie_to_nearest_even
          let overflow = F.Signed.Overflow.saturate
        end)
    ]} *)

open! Import

module type Round = sig
  module B : sig type t end
  type t
  val neg_infinity : t
  val pos_infinity : t
  val to_zero : t
  val away_from_zero : t
  val tie_to_neg_infinity : t
  val tie_to_pos_infinity : t
  val tie_to_zero : t
  val tie_away_from_zero : t
  val tie_to_nearest_even : t
  val tie_to_nearest_odd : t
  val generic : B.t -> t
  val eval : t -> int -> B.t -> B.t
end

module type Overflow = sig
  module B : sig type t end
  type t
  val wrap : t
  val saturate : t
  val eval : t -> int -> int -> B.t -> B.t
end

module type Fixed_point = sig
  module B : sig type t end

  type t

  val mk : int -> B.t -> t
  (** create a fixed point value.  [mk f x] will have [f] fractional bits.  [width x -
      f] will be the number of integer bits *)

  val int : t -> B.t
  (** return the integer part of the value *)

  val frac : t -> B.t
  (** return the fractional part of the value *)

  val signal : t -> B.t
  (** return the underlying bits *)

  val width_int : t -> int
  (** number of integer bits *)

  val width_frac : t -> int
  (** number of fractional bits *)

  val to_float : t -> float
  (** convert fixed point value to a float *)

  val select_int : t -> int -> B.t
  (** [select_int f x] extracts the integer part, and resizes it to x bits.  Bits are
      dropped from the msb down, if required. *)

  val select_frac : t -> int -> B.t
  (** [select_frac f x] extracts the fractional part, and resizes it to x bits.  Bits
      are dropped from the lsb up, if required. *)

  val select : t -> int -> int -> t
  (** resizes a fixed type using select_int and select_frac *)

  val norm : t list -> t list
  (** find largest integer and fractional parts in each fixed value, and resize all
      elements to that size *)

  val norm2 : t -> t -> t * t
  (** same as norm, but for 2 values *)

  val const : int -> int -> float -> t
  (** create a fixed value with the given number of integer and fractional bits from the
      floating point value *)

  val (+:) : t -> t -> t
  (** adition *)

  val (-:) : t -> t -> t
  (** subtraction *)

  val ( *: ) : t -> t -> t
  (** multiplication *)

  val (==:) : t -> t -> B.t
  (** equality *)

  val (<>:) : t -> t -> B.t
  (** inequality *)

  val (<:) : t -> t -> B.t
  (** less than *)

  val (<=:) : t -> t -> B.t
  (** less than or equal to *)

  val (>:) : t -> t -> B.t
  (** greater than *)

  val (>=:) : t -> t -> B.t
  (** greater than or equal to *)

  val mux : B.t -> t list -> t
  (** multiplexor *)

  val resize : t -> int -> int -> t
  (** [resize x i f] will resize the integer part to have [i] bits, and fractional part
      to have [f] bits.  Rounding and overflow control is applied *)
end

module type Fixed = sig

  module Make (B : Comb.S) : sig

    type unsigned
    type signed

    type 'a round
    type 'a overflow

    (** various different rounding modes *)
    module type Round = Round with module B := B

    (** overflow control - wrap or saturate *)
    module type Overflow = Overflow with module B := B

    (** fixed point API *)
    module type Fixed = Fixed_point with module B := B

    module Unsigned : sig
      module Round : (Round with type t = unsigned round)
      module Overflow : (Overflow with type t = unsigned overflow)
      module type Spec = sig
        val round : Round.t
        val overflow : Overflow.t
      end
      module Make (S : Spec) : Fixed
    end

    module Signed : sig
      module Round : (Round with type t = signed round)
      module Overflow : (Overflow with type t = signed overflow)
      module type Spec = sig
        val round : Round.t
        val overflow : Overflow.t
      end
      module Make (S : Spec) : Fixed
    end
  end
end
