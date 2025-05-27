open! Base
open! Hardcaml

module Explicit : sig
  val create : clock:Signal.t -> clear:Signal.t -> d:Signal.t -> Signal.t
end

module Generic : sig
  val create
    :  sequence:Bits.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> d:Signal.t
    -> Signal.t
end

module Fixed : sig
  val create
    :  sequence:Bits.t
    -> clock:Signal.t
    -> clear:Signal.t
    -> d:Signal.t
    -> Signal.t
end
