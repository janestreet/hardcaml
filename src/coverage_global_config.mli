(** Global coverage configuration that controls if and how coverage analysis will be run *)

(** Whether or not coverage of any kind is enabled *)
val coverage_enabled : unit -> bool

module For_cyclesim_coverage : sig
  val exe_coverage_enabled : unit -> bool
  val set_expect_test_mode : ?verbose:bool -> unit -> unit
  val verbose : unit -> bool
end
