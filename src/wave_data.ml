open Core0

module type S = Wave_data_intf.S

module Type = struct
  type t =
    | Input
    | Output
    | Internal
  [@@deriving compare ~localize, equal ~localize, sexp_of]
end

module Wave = struct
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

let combine a b =
  match a, b with
  | By_cycle a, By_cycle b -> By_cycle (Array.concat [ a; b ])
  | By_event a, By_event b -> By_event (Array.concat [ a; b ])
  | _ -> raise_s [%message "Cannot combine waveforms with different types"]
;;
