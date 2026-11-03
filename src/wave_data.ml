open Core0

module type%template [@mode m = (local, global)] S = Wave_data_intf.S [@mode m]

module Type = struct
  type t =
    | Input
    | Output
    | Internal
  [@@deriving bin_io, compare ~localize, equal ~localize, sexp_of]
end

module Wave = struct
  type 'wave_data t =
    { name : string
    ; width : int
    ; typ : Type.t
    ; wave_format : Wave_format.t
    ; wave_data : 'wave_data
    }
  [@@deriving bin_io, equal ~localize, sexp_of]
end

type t =
  | By_mutable_event of Wave_data_in_mutable_events.t Wave.t array
  | By_mutable_event_four_state of Wave_data_in_mutable_events_four_state.t Wave.t array
[@@deriving equal ~localize, sexp_of]

let to_mutable_events = function
  | By_mutable_event waves -> waves
  | By_mutable_event_four_state _ ->
    raise_s [%message "[to_mutable_events] does not support 4-state waveforms"]
;;

(* Drop the 4-state information from each event: a bit that is X or Z (mask set) becomes
   0, pure bits are unchanged. Aliased waves that share their wave data also share it
   after demotion.

   The demoted stores do not share the source's [max_time] ref - they are a snapshot, so
   they must not observe later mutations of the input - but each is seeded with the
   source's length so the demoted waveform spans the same time range. *)
let demote_to_two_state (waves : Wave_data_in_mutable_events_four_state.t Wave.t array)
  : Wave_data_in_mutable_events.t Wave.t array
  =
  let demoted_by_unique_id = Hashtbl.create (module Int) in
  Array.map waves ~f:(fun wave ->
    let wave_data =
      Hashtbl.find_or_add
        demoted_by_unique_id
        (Wave_data_in_mutable_events_four_state.unique_id wave.wave_data)
        ~default:(fun () ->
          let max_time =
            ref (Wave_data_in_mutable_events_four_state.length wave.wave_data - 1)
          in
          let demoted = Wave_data_in_mutable_events.create wave.width max_time in
          Wave_data_in_mutable_events_four_state.iter
            wave.wave_data
            ~f:(fun time ~bits ~mask ->
              Wave_data_in_mutable_events.insert demoted time Bits.(bits &: ~:mask));
          demoted)
    in
    { wave with wave_data })
;;

let to_two_state t =
  match t with
  | By_mutable_event _ -> t
  | By_mutable_event_four_state waves -> By_mutable_event (demote_to_two_state waves)
;;

(* Promote each event to 4-state with an all-zeros mask: every bit of a 2-state wave is
   pure, so promotion is lossless (unlike [demote_to_two_state]). Aliased waves that share
   their wave data also share it after promotion.

   As with demotion, the promoted stores do not share the source's [max_time] ref - they
   are a snapshot seeded with the source's length. *)
let promote_to_four_state (waves : Wave_data_in_mutable_events.t Wave.t array)
  : Wave_data_in_mutable_events_four_state.t Wave.t array
  =
  let promoted_by_unique_id = Hashtbl.create (module Int) in
  Array.map waves ~f:(fun wave ->
    let wave_data =
      Hashtbl.find_or_add
        promoted_by_unique_id
        (Wave_data_in_mutable_events.unique_id wave.wave_data)
        ~default:(fun () ->
          let max_time = ref (Wave_data_in_mutable_events.length wave.wave_data - 1) in
          let promoted =
            Wave_data_in_mutable_events_four_state.create wave.width max_time
          in
          Wave_data_in_mutable_events.iter wave.wave_data ~f:(fun time bits ->
            Wave_data_in_mutable_events_four_state.insert
              promoted
              time
              ~bits
              ~mask:(Bits.zero (Bits.width bits)));
          promoted)
    in
    { wave with wave_data })
;;

let to_four_state t =
  match t with
  | By_mutable_event waves -> By_mutable_event_four_state (promote_to_four_state waves)
  | By_mutable_event_four_state _ -> t
;;

let combine a b =
  match a, b with
  | By_mutable_event a, By_mutable_event b -> By_mutable_event (Array.concat [ a; b ])
  | By_mutable_event_four_state a, By_mutable_event_four_state b ->
    By_mutable_event_four_state (Array.concat [ a; b ])
  | By_mutable_event a, By_mutable_event_four_state b ->
    By_mutable_event_four_state (Array.concat [ promote_to_four_state a; b ])
  | By_mutable_event_four_state a, By_mutable_event b ->
    By_mutable_event_four_state (Array.concat [ a; promote_to_four_state b ])
;;

module For_file_serialization = struct
  type 'wave_data t =
    { wave_datas : 'wave_data array
    ; waves : int Wave.t array
    }
  [@@deriving bin_io]

  (* Checked when writing as well as reading, so a wave whose [width] field disagrees with
     its wave data fails where the bug is rather than when the file is loaded. *)
  let check_width (wave : _ Wave.t) ~width =
    if not (Int.equal wave.width (width wave.wave_data))
    then
      raise_s
        [%message
          "Wave width does not match its wave data"
            ~name:(wave.name : string)
            ~wave_width:(wave.width : int)
            ~data_width:(width wave.wave_data : int)]
  ;;

  let of_waves waves ~unique_id ~width =
    let wave_datas = Queue.create () in
    let index_of_unique_id = Hashtbl.create (module Int) in
    let waves =
      Array.map waves ~f:(fun (wave : _ Wave.t) ->
        check_width wave ~width;
        let wave_data =
          Hashtbl.find_or_add
            index_of_unique_id
            (unique_id wave.wave_data)
            ~default:(fun () ->
              Queue.enqueue wave_datas wave.wave_data;
              Queue.length wave_datas - 1)
        in
        { wave with wave_data })
    in
    { wave_datas = Queue.to_array wave_datas; waves }
  ;;

  let to_waves { wave_datas; waves } ~width =
    let num_wave_datas = Array.length wave_datas in
    Array.map waves ~f:(fun (wave : _ Wave.t) ->
      let index = wave.wave_data in
      if index < 0 || index >= num_wave_datas
      then
        raise_s
          [%message
            "Invalid serialized wave data index" (index : int) (num_wave_datas : int)];
      let wave_data = wave_datas.(index) in
      if not (Int.equal wave.width (width wave_data))
      then
        raise_s
          [%message
            "Serialized wave width does not match its wave data"
              ~name:(wave.name : string)
              ~wave_width:(wave.width : int)
              ~data_width:(width wave_data : int)];
      { wave with wave_data })
  ;;
end

type event =
  { wave_index : int
  ; event_index : int
  }

let event_sequence_in_time_order waves ~num_events ~time_at_index : event Sequence.t =
  (* Min-heap keyed by [time]; elements track which wave the entry came from and which
     event index within that wave it refers to. *)
  let heap =
    Pairing_heap.create ~cmp:(fun (t1, _, _) (t2, _, _) -> Int.compare t1 t2) ()
  in
  Array.iteri waves ~f:(fun wave_index wave ->
    if num_events wave > 0 then Pairing_heap.add heap (time_at_index wave 0, wave_index, 0));
  Sequence.unfold ~init:() ~f:(fun () ->
    match Pairing_heap.pop heap with
    | None -> None
    | Some (_time, wave_index, event_index) ->
      let next_event_index = event_index + 1 in
      if next_event_index < num_events waves.(wave_index)
      then
        Pairing_heap.add
          heap
          (time_at_index waves.(wave_index) next_event_index, wave_index, next_event_index);
      Some ({ wave_index; event_index }, ()))
;;
