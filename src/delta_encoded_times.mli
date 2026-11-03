(** Event-time arrays whose [bin_io] representation delta-encodes [data]. In memory,
    [data] contains absolute times; its generally small deltas serialize more compactly. *)

open! Core0

type t =
  { used_length : int
  ; data : int array
  }
[@@deriving bin_io]
