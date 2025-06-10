(** Module for connecting the coverage to a hardcaml simulator. *)

val maybe_wrap : ('i, 'o) Cyclesim0.t -> Circuit.t -> ('i, 'o) Cyclesim0.t
