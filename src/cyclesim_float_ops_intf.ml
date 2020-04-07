open! Import

module type Real = sig
  val database : Combinational_ops_database.t
  val ( +: ) : Signal.t -> Signal.t -> Signal.t
  val ( -: ) : Signal.t -> Signal.t -> Signal.t
  val ( *: ) : Signal.t -> Signal.t -> Signal.t
  val ( /: ) : Signal.t -> Signal.t -> Signal.t
  val ( %: ) : Signal.t -> Signal.t -> Signal.t
  val ( **: ) : Signal.t -> Signal.t -> Signal.t
  val exp : Signal.t -> Signal.t
  val log : Signal.t -> Signal.t
  val log10 : Signal.t -> Signal.t
  val cos : Signal.t -> Signal.t
  val sin : Signal.t -> Signal.t
  val tan : Signal.t -> Signal.t
  val acos : Signal.t -> Signal.t
  val asin : Signal.t -> Signal.t
  val atan : Signal.t -> Signal.t
  val atan2 : Signal.t -> Signal.t -> Signal.t
  val cosh : Signal.t -> Signal.t
  val sinh : Signal.t -> Signal.t
  val tanh : Signal.t -> Signal.t
  val ceil : Signal.t -> Signal.t
  val floor : Signal.t -> Signal.t
  val abs : Signal.t -> Signal.t
end

module type Cyclesim_float_ops = sig
  module type Real = Real

  (** 32 bit floats *)
  module Float : Real

  (** 64 bit doubles *)
  module Double : Real
end
