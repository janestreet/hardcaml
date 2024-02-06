(** A [Cyclesim] implementation where the simulation data is packed into a single array. *)

module Config = Cyclesim0.Config

val create : ?config:Config.t -> Circuit.t -> Cyclesim0.t_port_list
