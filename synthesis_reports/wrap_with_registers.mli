(** Take a general design and wrap the input and output ports with registers. This should
    only be used for combinational logic circuits (sequential circuits should perform
    their own wrapping). This helps to get realistic timing reports from the synthesis
    run. *)
open Hardcaml

module Make (I : Interface.S) (O : Interface.S) : sig
  module I_with_clock : Interface.S

  val create
    :  (Scope.t -> Signal.t Interface.Create_fn(I)(O).t)
    -> Scope.t
    -> Signal.t Interface.Create_fn(I_with_clock)(O).t

  val hier
    :  name:string
    -> (Scope.t -> Signal.t Interface.Create_fn(I)(O).t)
    -> Scope.t
    -> Signal.t Interface.Create_fn(I_with_clock)(O).t
end
