(** {2 Wallace/Dadda tree multipliers}

    The Wallace/Dadda tree has three steps:

    1. Multiply (that is - AND) each bit of one of the arguments, by each bit of the
    other, yielding n^2 results.  Depending on position of the multiplied bits, the wires
    carry different weights, for example wire of bit carrying result of a2b3 is 32.

    2. Reduce the number of partial products to two by layers of full and half adders.

    3. Group the wires in two numbers, and add them with a conventional adder.

    They differ in step 2 in how the results are combined.

    {3 wallace}

    - Take any three wires with the same weights and input them into a full adder.  The
      result will be an output wire of the same weight and an output wire with a higher
      weight for each three input wires.
    - If there are two wires of the same weight left, input them into a half adder.
    - If there is just one wire left, connect it to the next layer.

    {3 dadda}

    - Take any three wires with the same weights and input them into a full adder.  The
      result will be an output wire of the same weight and an output wire with a higher
      weight for each three input wires.
    - If there are 2 wires of the same weight left, and the current number of output wires
      with that weight is equal to 2 (modulo 3), input them into a half adder. Otherwise,
      pass them through to the next layer.
    - If there is just 1 wire left, connect it to the next layer.

    However, when a layer carries at most 3 input wires for any weight, that layer will be
    the last one.  In this case, the Dadda tree will use half adder more aggressively (but
    still not as much as in a Wallace multiplier), to ensure that there are only two
    outputs for any weight.  Then, the second rule above changes as follows:

    - If there are 2 wires of the same weight left, and the current number of output wires
      with that weight is equal to 1 or 2 (modulo 3), input them into a half adder.
      Otherwise, pass them through to the next layer. *)

open! Import

module type Gen = sig
  include Add.Gen

  val width : t -> int
  val bit : t -> int -> bit
  val gnd : bit
  val (+:) : t -> t -> t
  val uresize : t -> int -> t
end

module type Mul = sig
  module type Gen = Gen

  module Config : sig
    type t =
      | Dadda
      | Wallace
    [@@deriving enumerate, sexp_of]
  end

  val create_gen
    :  config : Config.t
    -> (module Gen with type t = 'a)
    -> 'a
    -> 'a
    -> 'a

  val create
    :  config : Config.t
    -> (module Comb.S with type t = 'a)
    -> 'a
    -> 'a
    -> 'a
end
