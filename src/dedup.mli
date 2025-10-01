(** Deduplicates combinatorial nodes performing redundant computation. *)

open! Core0

val deduplicate : Circuit.t -> Circuit.t

module For_testing : sig
  val signal_hash : (Signal.Type.Uid.t, int) Hashtbl.t -> Signal.t -> int
end
