(** ROM-accumulator.

    Evaluate [ a0.x0 + a1.x1 + ... + an.xn ] where the [ai] are constants, using
    distributed arithmetic.  The architecture uses a rom and add/shift circuit and
    requires no multipliers.

    The ROM-accumulator extends the idea of multiplication by adding and shifting.
    [a0.x0] can be the calculated by testing each bit of [x] and adding [a] to the shifted
    accumulator.  Similarly [a0.x0 + a1.x1] can be calculated by forming an address vector
    from each successive bit, [b], of x0 and x1, i.e.:

    [[x1.[b]; x0.[b]]]

    A pre-calculated rom stores all possibile additions of a0 and a1

    [[ 0; a0; a1; a0+a1 ]]

    Given n coefficients the required rom will be of size 2^n.

    The address is looked up in the rom and added to (or subtracted from) the shifted
    accumulator. *)

open! Import

module Mode : sig
  (** In [integer] mode the coefficients and accumulator are treated as integers, the
      internal shift registers shift msb first, and the accumulator shifts to the left.
      This in turn specifies an exact result, so long as the accumulator is large enough
      to hold it.

      In fixed mode the coefficients and accumulator are treated as fixed point values,
      the shift regiters shift lsb first and the accumulator shifts to the right. *)
  type t =
    | Fixed
    | Integer
  [@@deriving sexp_of]
end

module type Config = sig
  val mode             : Mode.t
  (** Width of the assumulator. *)
  val accumulator_bits : int
  (** Width of input data. *)
  val data_bits        : int
  (** Number of coefficients. *)
  val num_coefs        : int
  (** Extra least significant bits added to the accumulator.  This can add extra
      precision without extending the rom size. *)
  val rom_shift        : int
end

module Make (Config : Config) : sig
  module I : sig
    type 'a t =
      { clk    : 'a
      ; clr    : 'a
      ; en     : 'a
      ; ld     : 'a (* Load input data to internal shift registers *)
      ; addsub : 'a (* High on the msb (if input data is signed).  *)
      ; x      : 'a array
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O : sig
    type 'a t =
      { q : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  (** Create the Rom-accumulator. *)
  val create
    :  coefs : Bits.t array
    -> Signal.t I.t
    -> Signal.t O.t
end
