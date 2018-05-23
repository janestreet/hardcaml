(** Low level adder components *)

open! Import

(** [Gen] is a (massively) cut down [Comb.S] that distinguishes between the type of a
    single bit, [bit], and a sequence of bits, [t].  This interface makes it clear that
    constructions only use single-bit primitive operations. *)
module type Gen = sig
  type bit [@@deriving sexp_of]
  type t [@@deriving sexp_of]
  val (&:) : bit -> bit -> bit
  val (|:) : bit -> bit -> bit
  val (^:) : bit -> bit -> bit
  val ( ~: ) : bit -> bit
  val bits : t -> bit list
  val concat : bit list -> t
end

module type S = sig

  module B : Gen

  module Add_result : sig
    type t =
      { carry : B.bit
      ; sum   : B.bit }
    [@@deriving sexp_of]
  end

  (** Make [Add_result] fields available directly. *)
  type add_result = Add_result.t =
    { carry : B.bit
    ; sum   : B.bit }

  (** [full_adder] inputs and outputs are single bits *)
  val full_adder : B.bit -> B.bit -> B.bit -> Add_result.t

  (** [half_adder] inputs and outputs are single bits *)
  val half_adder : B.bit -> B.bit -> Add_result.t

  module Subtract_result : sig
    type t =
      { borrow : B.bit
      ; sub    : B.bit }
    [@@deriving sexp_of]
  end

  (** Make [Subtract_result] fields available directly. *)
  type subtract_result = Subtract_result.t =
    { borrow : B.bit
    ; sub    : B.bit }

  (** [subtractor] inputs and outputs are single bits *)
  val subtractor : B.bit -> B.bit -> B.bit -> Subtract_result.t

  (** [ripple_carry_adder] inputs are [Bits.t]s of the same width, and a single-bit
      [carry_in_bit].  The output has one more bit than the inputs. *)
  val ripple_carry_adder : B.t -> B.t -> carry_in_bit:B.bit -> B.t
end

module type Add = sig

  module type Gen = Gen

  module Make (B : Comb.S) : S
    with type B.t := B.t
    with type B.bit := B.t

  module Make_gen (B : Gen) : S with module B := B
end
