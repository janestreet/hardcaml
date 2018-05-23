open! Import

module Make (Config : Rac.Config) : sig
  module Rac  : module type of Rac.Make(Config)
  module Step : module type of Hardcaml_step_testbench.Make(Rac.I)(Rac.O)
  module Sim  : module type of Cyclesim.With_interface(Rac.I)(Rac.O)

  val test
    :  ?print : bool
    -> unit
    -> coefs : int array
    -> data_in : int array
    -> int
end
