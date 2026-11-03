(** Waveform data interface. *)

open Core0

module type S = sig
  type t [@@deriving sexp_of, equal]

  val width : t -> int
  val length : t -> int
  val num_events : t -> int
  val find_insertion_index : t -> int -> int option
  val get_time_at_index : t -> int -> int

  (** The value of the event at the given index. For 4-state data this is the [bits]
      value, in which bits with a non-zero mask encode X (1) or Z (0) rather than a
      logical value. *)
  val get_data_at_index : t -> int -> Bits.t

  (** [true] if this wave data stores 4-state (0, 1, X, Z) values. Renderers should only
      consult the mask value when this is [true], keeping the (common) 2-state render path
      free of mask handling. Constant per [t]; hoist the check out of loops. *)
  val is_four_state : t -> bool

  (** The mask value of the event at the given index: a set bit marks an X or Z in the
      corresponding position of [get_data_at_index]. All zeros when [is_four_state] is
      [false]. *)
  val get_mask_at_index : t -> int -> Bits.t

  val get : t -> int -> Bits.t
  val get_digestible_string : t -> Bytes.t * int
end

module type Wave_data = sig
  module type S = S

  module Type : sig
    type t =
      | Input
      | Output
      | Internal
    [@@deriving compare, equal, sexp_of]
  end

  module Wave : sig
    type 'wave_data t =
      { name : string
      ; width : int
      ; typ : Type.t
      ; wave_format : Wave_format.t
      ; wave_data : 'wave_data
      }
    [@@deriving equal, sexp_of]
  end

  (** [equal] is structural, and compares across constructors: a 2-state waveform and the
      4-state waveform holding the same values (with all-zero masks) are not equal. *)
  type t =
    | By_mutable_event of Wave_data_in_mutable_events.t Wave.t array
    | By_mutable_event_four_state of Wave_data_in_mutable_events_four_state.t Wave.t array
  [@@deriving equal, sexp_of]

  (** Returns the waves of a [By_mutable_event] waveform. Raises on the 4-state
      constructor. *)
  val to_mutable_events : t -> Wave_data_in_mutable_events.t Wave.t array

  (** Convert a waveform to the 2-state representation, for consumers that cannot handle X
      / Z.

      This is {e lossy}: every impure bit is replaced by 0, so X and Z both read back as
      0. There is no way to tell a demoted X from a real 0 in the result. *)
  val to_two_state : t -> t

  (** Convert a waveform to the 4-state representation.

      This is lossless: every bit of a 2-state wave is pure, so the promoted waves carry
      an all-zeros mask. The promoted wave data is a snapshot: it does not observe later
      mutations of the input. *)
  val to_four_state : t -> t

  (** Concatenate the waves of two waveforms.

      Combining two waveforms of the same format aliases the inputs: the waves are
      concatenated as-is, with no copying, so the combined waveform observes subsequent
      mutations of either input's wave data.

      Mixing a 2-state and a 4-state waveform promotes the 2-state side as in
      [to_four_state] (losslessly) and yields a 4-state waveform. The promoted side is a
      snapshot rather than an alias; the 4-state side still aliases its input. *)
  val combine : t -> t -> t

  (** Bin-io-able payload for on-disk serialization of a waveform's waves (see
      [Hardcaml_waveterm.Serialize_waveform], which brands the payload for each wave-data
      type with a distinct magic header).

      Aliased waves (the same waveform traced under multiple names) physically share their
      [wave_data]. The payload serializes each shared allocation once and stores which
      data index a wave corresponds to in [waves]; [to_waves] restores the sharing. *)
  module For_file_serialization : sig
    type 'wave_data t [@@deriving bin_io]

    (** Raises if a wave's [width] does not match the width of its [wave_data], so that
        the mismatch is reported when the bad waveform is written rather than when it is
        later read back. *)
    val of_waves
      :  'wave_data Wave.t array
      -> unique_id:('wave_data -> int)
      -> width:('wave_data -> int)
      -> 'wave_data t

    (** Validates the payload (data indices in range, wave widths matching their data) and
        raises on invalid input. *)
    val to_waves : 'wave_data t -> width:('wave_data -> int) -> 'wave_data Wave.t array
  end

  type event =
    { wave_index : int
    ; event_index : int
    }

  (** Iterate over events from all waves in increasing time order. The events within each
      wave are assumed to already be in increasing time order. *)
  val event_sequence_in_time_order
    :  'wave_data Wave.t array
    -> num_events:('wave_data Wave.t -> int)
    -> time_at_index:('wave_data Wave.t -> int -> int)
    -> event Sequence.t
end
