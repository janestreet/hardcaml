(** Waveform data interface. *)

open Core0

module type S = sig
  type t [@@deriving sexp_of, equal ~localize]

  val width : t -> int
  val length : t -> int
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
    [@@deriving compare ~localize, equal ~localize, sexp_of]
  end

  module Wave : sig
    type 'wave_data t =
      { name : string
      ; width : int
      ; typ : Type.t
      ; wave_format : Wave_format.t
      ; is_pseudo_clock : bool
      ; wave_data : 'wave_data
      }
    [@@deriving equal ~localize, sexp_of]
  end

  type t =
    | By_cycle of Wave_data_in_cycles.t Wave.t array
    | By_event of Wave_data_in_events.Bits.t Wave.t array
  [@@deriving equal ~localize, sexp_of]

  val combine : t -> t -> t

  type event =
    { wave_index : int
    ; event_index : int
    }

  (** Iterate over events from all waves in increasing time order. The input waves are
      assumed to already be in increasing time order individually. *)
  val event_sequence_in_time_order
    :  Wave_data_in_events.Bits.t Wave.t array
    -> event Sequence.t
end
