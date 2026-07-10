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

type event =
  { wave_index : int
  ; event_index : int
  }

let event_sequence_in_time_order (waves : Wave_data_in_events.Bits.t Wave.t array)
  : event Sequence.t
  =
  let event_store wave_index =
    Wave_data_in_events.Bits.event_store waves.(wave_index).wave_data
  in
  let num_events wave_index =
    Wave_data_in_events.Bits.Event_store.length (event_store wave_index)
  in
  let time_at wave_index event_index =
    Wave_data_in_events.Bits.Event_store.get_time_at_index
      (event_store wave_index)
      event_index
  in
  (* Min-heap keyed by [time]; elements track which wave the entry came from and which
     event index within that wave it refers to. *)
  let heap =
    Pairing_heap.create ~cmp:(fun (t1, _, _) (t2, _, _) -> Int.compare t1 t2) ()
  in
  Array.iteri waves ~f:(fun wave_index _ ->
    if num_events wave_index > 0
    then Pairing_heap.add heap (time_at wave_index 0, wave_index, 0));
  Sequence.unfold ~init:() ~f:(fun () ->
    match Pairing_heap.pop heap with
    | None -> None
    | Some (_time, wave_index, event_index) ->
      let next_event_index = event_index + 1 in
      if next_event_index < num_events wave_index
      then
        Pairing_heap.add
          heap
          (time_at wave_index next_event_index, wave_index, next_event_index);
      Some ({ wave_index; event_index }, ()))
;;
