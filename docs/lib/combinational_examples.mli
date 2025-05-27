open Hardcaml

module Rom : sig
  val rom : address:Signal.t -> Signal.t
end

module Bin_to_gray : sig
  val bin_to_gray_1 : Signal.t -> Signal.t
  val bin_to_gray_2 : Signal.t -> Signal.t
end

module Priority_encoder : sig
  type fn =
    sel:Signal.t -> a:Signal.t -> b:Signal.t -> c:Signal.t -> d:Signal.t -> Signal.t

  val priority_encoder_1 : fn
  val priority_encoder_2 : fn
end

module Mux4 : sig
  val mux4
    :  address:Signal.t
    -> a:Signal.t
    -> b:Signal.t
    -> c:Signal.t
    -> d:Signal.t
    -> Signal.t
end

module Full_add : sig
  val full_add_1 : a:Signal.t -> b:Signal.t -> cin:Signal.t -> Signal.t * Signal.t
  val full_add_2 : a:Signal.t -> b:Signal.t -> cin:Signal.t -> Signal.t * Signal.t
end

module Parity : sig
  type fn = d:Signal.t -> Signal.t

  val parity_1 : fn
  val parity_2 : fn
  val parity_3 : fn
end

module Alu : sig
  val alu : op:Signal.t -> a:Signal.t -> b:Signal.t -> Signal.t

  module I : Interface.S
  module O : Interface.S

  val typed_alu : Interface.Create_fn(I)(O).t
end
