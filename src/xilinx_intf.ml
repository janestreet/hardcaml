(** Basic Xilinx FPGA primitives *)

open! Import

module type S = sig
  val lut : int64 -> Signal.t -> Signal.t
  val muxcy : Signal.t -> Signal.t -> Signal.t -> Signal.t
  val inv : Signal.t -> Signal.t
  val xorcy : Signal.t -> Signal.t -> Signal.t
  val muxf5 : Signal.t -> Signal.t -> Signal.t -> Signal.t
  val muxf6 : Signal.t -> Signal.t -> Signal.t -> Signal.t
  val muxf7 : Signal.t -> Signal.t -> Signal.t -> Signal.t
  val muxf8 : Signal.t -> Signal.t -> Signal.t -> Signal.t
  val fdce : Signal.t -> Signal.t -> Signal.t -> Signal.t -> Signal.t
  val fdpe : Signal.t -> Signal.t -> Signal.t -> Signal.t -> Signal.t
  val mult_and : Signal.t -> Signal.t -> Signal.t
  val ram1s : Signal.t -> Signal.t -> Signal.t -> Signal.t -> Signal.t
end

module type T = sig
  module LutEqn : sig
    type t
  end

  val x_lut : LutEqn.t -> Signal.t -> Signal.t
  val x_map : LutEqn.t -> Signal.t list -> Signal.t
  val x_and : Signal.t -> Signal.t -> Signal.t
  val x_or : Signal.t -> Signal.t -> Signal.t
  val x_xor : Signal.t -> Signal.t -> Signal.t
  val x_not : Signal.t -> Signal.t

  val x_reduce_carry
    :  bool
    -> (LutEqn.t -> LutEqn.t -> LutEqn.t)
    -> Signal.t
    -> Signal.t
    -> Signal.t
    -> Signal.t

  val x_and_reduce : Signal.t -> Signal.t
  val x_or_reduce : Signal.t -> Signal.t
  val x_reduce_tree : (LutEqn.t -> LutEqn.t -> LutEqn.t) -> Signal.t -> Signal.t
  val x_add_carry : LutEqn.t -> Signal.t -> Signal.t -> Signal.t -> Signal.t * Signal.t
  val x_add : Signal.t -> Signal.t -> Signal.t
  val x_sub : Signal.t -> Signal.t -> Signal.t

  val x_mux_add_carry
    :  LutEqn.t
    -> Signal.t
    -> Signal.t
    -> Signal.t * Signal.t
    -> Signal.t
    -> Signal.t * Signal.t

  (** [x_mux_add x (a, a') b] gives [(x ? a : a') + b] *)
  val x_mux_add : Signal.t -> Signal.t * Signal.t -> Signal.t -> Signal.t

  (** [x_mux_sub x a (b, b')] gives [a - (x ? b : b')] *)
  val x_mux_sub : Signal.t -> Signal.t -> Signal.t * Signal.t -> Signal.t

  val x_eq : Signal.t -> Signal.t -> Signal.t
  val x_lt : Signal.t -> Signal.t -> Signal.t
  val x_mux : Signal.t -> Signal.t list -> Signal.t
  val x_mulu : Signal.t -> Signal.t -> Signal.t
  val x_muls : Signal.t -> Signal.t -> Signal.t
end

module type Xilinx = sig
  module type S = S

  (** Allow expressions to generate LUT init values *)
  module LutEqn : sig
    type t

    val i0 : t
    val i1 : t
    val i2 : t
    val i3 : t
    val i4 : t
    val i5 : t
    val gnd : t
    val vdd : t
    val ( &: ) : t -> t -> t
    val ( |: ) : t -> t -> t
    val ( ^: ) : t -> t -> t
    val ( ~: ) : t -> t
    val ( ==: ) : t -> t -> t
    val ( <>: ) : t -> t -> t
    val eval : int -> t -> int64
  end

  (** Hardcaml simulation based models of Xilinx primitives *)
  module Hardcaml_api : S

  (** Unisim library based Xilinx primitives *)
  module Unisim : S

  module type T = T with module LutEqn := LutEqn

  module type LutSize = sig
    val max_lut : int
  end

  module Lut4 : LutSize
  module Lut6 : LutSize
  module XMake (X : S) (L : LutSize) : T
  module XComb (Synth : T) : Comb.Primitives with type t = Signal.t

  (** combinatorial only transform *)
  module XSynthesizeComb (X : S) (L : LutSize) : Transform.TransformFn

  (** sequential and combinatorial transform TODO memories *)
  module XSynthesize (X : S) (L : LutSize) : Transform.TransformFn
end
