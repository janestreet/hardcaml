(** Deduplicates combinatorial nodes performing redundant computation. *)

val deduplicate : Circuit.t -> Circuit.t

module For_testing : sig
  val signal_hash : (Signal.Type.Uid.t, int) Base.Hashtbl.t -> Signal.t -> int
end
