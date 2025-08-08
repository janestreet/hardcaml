(** Signal coverage information associated with a specific signal + Cyclesim run.

    Tracks information on how to sample the signals current value and where to record
    coverage stats. *)

type t

val create : _ Cyclesim0.t -> Signal.t -> Signal_coverage.t -> t
val sample : t -> unit
