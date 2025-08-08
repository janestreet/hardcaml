(** Module for connecting the coverage to a hardcaml simulator. *)

module For_cyclesim : sig
  val maybe_wrap : ('i, 'o) Cyclesim0.t -> Circuit.t -> ('i, 'o) Cyclesim0.t
end

module For_expect_tests : sig
  val enable_and_maybe_reset : unit -> unit
  val output_results : unit -> unit
  val output_compact_results : unit -> unit
end
