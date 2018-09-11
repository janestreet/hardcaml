open! Import

module Make (Config : Rac.Config) : sig
  module Rac  : module type of Rac.Make(Config)
  module Step : module type of Hardcaml_step_testbench.Make(Rac.I)(Rac.O)
  module Sim  : module type of Cyclesim.With_interface(Rac.I)(Rac.O)

  val create_sim : coefs : int array -> Sim.t

  val testbench : data_in : int array -> _ -> int Step.t

  val run
    :  simulator : Sim.t
    -> testbench : (data_in : int array -> Bits.t Rac.O.t -> int Step.t)
    -> data_in : int array
    -> int

  val test
    :  ?print : bool
    -> unit
    -> coefs : int array
    -> data_in : int array
    -> int
end
