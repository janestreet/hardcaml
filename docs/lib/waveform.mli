open Base
open Hardcaml
module I : Interface.S
module O : Interface.S

val create : Signal.t I.t -> Signal.t O.t

module Simulator : module type of Cyclesim.With_interface (I) (O)

val testbench : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t -> unit
