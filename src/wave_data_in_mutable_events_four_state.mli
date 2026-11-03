(** Like [Wave_data_in_mutable_events], but stores 4-state (0, 1, X, Z) values.

    Each event holds two [Bits.t] values of equal width:

    - [bits]: the logical value of each bit
    - [mask]: per-bit impurity. Where [mask=0], [bits] holds the real 0/1 value. Where
      [mask=1], the bit is impure: [bits=1] means X and [bits=0] means Z.

    This matches the encoding used by the 4-state event driven simulator. The two values
    are stored separately so the [bits] value keeps the same packed representation (and
    read cache) as 2-state waveforms. *)

open! Core0

type t [@@deriving sexp_of, equal ~localize, bin_io]

(** [create width max_time] creates an empty store of [width]-bit events.

    [max_time] is shared, by design, between all the waves of one waveform: it is the
    largest time any of them has seen, and therefore the length of the waveform as a
    whole. *)
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
val set : t -> int -> time:int -> bits:Bits.t -> mask:Bits.t -> unit

val%template get_time_at_index : t @ m -> int -> int [@@mode m = (global, local)]

(** The [bits] value of the event at the given index. Bits with a non-zero mask encode
    X (1) or Z (0) rather than a logical value. *)
val%template get_data_at_index : t @ m -> int -> Bits.t
[@@mode m = (global, local)]

(** Always [true]: this module stores 4-state values. See [Wave_data_intf.S]. *)
val is_four_state : t -> bool

(** The [mask] value of the event at the given index. *)
val%template get_mask_at_index : t @ m -> int -> Bits.t
[@@mode m = (global, local)]

val iter : t -> f:(int -> bits:Bits.t -> mask:Bits.t -> unit) -> unit
val find_insertion_index : t -> int -> int option
val insert : t -> int -> bits:Bits.t -> mask:Bits.t -> unit
val insert_value : t -> int -> Four_state_logic.t -> unit

(** Return the data at the given time. Along with [get_mask] returns [X] if there are no
    events at all. *)
val get : t -> int -> Bits.t

(** See [get]. *)
val get_mask : t -> int -> Bits.t

val get_digestible_string : t -> Bytes.t * int
