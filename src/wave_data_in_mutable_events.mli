open! Core0

type t [@@deriving sexp_of, equal ~localize, bin_io]

val create : int -> int ref -> t

(** Identifies the allocation of [t]. Aliased waves that share one [t] have equal unique
    ids, and distinct allocations (including deserialized ones) never do. Used to detect
    sharing without comparing contents. *)
val unique_id : t -> int

val width : t -> int

(** The number of time steps in the waveform, that is [max_time + 1] where [max_time] is
    the shared maximum passed to [create]. This is a property of the whole waveform, not
    of this wave: it is generally much larger than [num_events]. *)
val length : t -> int

(** The number of events actually recorded in this wave. *)
val num_events : t -> int

val capacity : t -> int
val resize : t -> unit
val set : t -> int -> time:int -> Bits.t -> unit
val set_mutable_unsafe : t -> int -> time:int -> Bits.Mutable.t -> unit

val%template get_time_at_index : t @ m -> int -> int [@@mode m = (global, local)]
val%template get_data_at_index : t @ m -> int -> Bits.t [@@mode m = (global, local)]

(** Always [false]: this module stores 2-state values. See [Wave_data_intf.S]. *)
val is_four_state : t -> bool

(** Always all-zeros. See [Wave_data_intf.S]. *)
val get_mask_at_index : t -> int -> Bits.t

(** Iterate over all events in time order, passing the time and data of each. *)
val iter : t -> f:(int -> Bits.t -> unit) -> unit

val find_insertion_index : t -> int -> int option
val insert : t -> int -> Bits.t -> unit
val insert_mutable_unsafe : t -> int -> Bits.Mutable.t -> unit
val insert_from_bytes : t -> int -> Bytes.t -> int -> unit
val get : t -> int -> Bits.t
val get_digestible_string : t -> Bytes.t * int
