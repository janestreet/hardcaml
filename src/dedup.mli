(** Deduplicates combinatorial nodes performing redundant computation. *)

open! Core0

val deduplicate : Circuit.t -> Circuit.t

module For_testing : sig
  val signal_hash : int Signal.Type.Uid.Table.t -> Signal.t -> int
end
