open! Core0

type data =
  { bits : Wave_data_in_cycles.t
  ; mask : Wave_data_in_cycles.t
  }

(* Data sorted in time order. Times are stored unboxed in an [int array], while the [bits]
   and [mask] values are each stored in a [Wave_data_in_cycles] so they use the same
   packed representation as 2-state traces. The two values always have the same length;
   events are only recorded through [set]/[insert], which write both. *)
type t = data Wave_data_in_mutable_events_kernel.t

include Wave_data_in_mutable_events_kernel.Shared_functions

let create width max_time =
  create
    ~create_data:(fun width ->
      { bits = Wave_data_in_cycles.create width; mask = Wave_data_in_cycles.create width })
    width
    max_time
;;

let set (t : t) index ~time ~bits ~mask =
  t.time.(index) <- time;
  Wave_data_in_cycles.set t.data.bits index bits;
  Wave_data_in_cycles.set t.data.mask index mask;
  t.length <- max t.length (index + 1);
  t.max_time := max !(t.max_time) time
;;

let%template[@mode m = (global, local)] get_data_at_index (t : t @ m) index =
  (Wave_data_in_cycles.get [@mode m]) t.data.bits index
;;

let%template[@mode m = (global, local)] get_mask_at_index (t : t @ m) index =
  (Wave_data_in_cycles.get [@mode m]) t.data.mask index
;;

let is_four_state _ = true

(* Content equality over the recorded events; unused capacity and cache state are ignored. *)
let%template[@mode m = (local, global)] equal (a : t @ m) (b @ m) =
  let rec events_equal index =
    index = a.length
    || (Int.equal
          ((get_time_at_index [@mode m]) a index)
          ((get_time_at_index [@mode m]) b index)
        && Bits.equal
             ((get_data_at_index [@mode m]) a index)
             ((get_data_at_index [@mode m]) b index)
        && Bits.equal
             ((get_mask_at_index [@mode m]) a index)
             ((get_mask_at_index [@mode m]) b index)
        && events_equal (index + 1))
  in
  phys_equal a b
  || (Int.equal a.width b.width
      && Int.equal !(a.max_time) !(b.max_time)
      && Int.equal a.length b.length
      && (events_equal 0 [@nontail]))
;;

let iter (t : t) ~f =
  for index = 0 to t.length - 1 do
    f
      (get_time_at_index t index)
      ~bits:(get_data_at_index t index)
      ~mask:(get_mask_at_index t index)
  done
;;

let sexp_of_t (t : t) =
  let time = List.init t.length ~f:(get_time_at_index t) in
  let data =
    List.init t.length ~f:(fun index ->
      Four_state_logic.create_exn
        ~bits:(get_data_at_index t index)
        ~mask:(get_mask_at_index t index)
      |> Four_state_logic.to_string)
  in
  (* Named [num_events] rather than [length]: [t.length] is the number of recorded events,
     whereas the [length] function returns [max_time + 1]. *)
  let num_events = t.length in
  let width = t.width in
  let max_time = !(t.max_time) in
  [%sexp
    { time : int list; data : string list; num_events : int; width : int; max_time : int }]
;;

let shuffle_up (t : t) index =
  for i = t.length downto index + 1 do
    set
      t
      i
      ~time:(get_time_at_index t (i - 1))
      ~bits:(get_data_at_index t (i - 1))
      ~mask:(get_mask_at_index t (i - 1))
  done
;;

let insert (t : t) time ~bits ~mask =
  if t.length = capacity t then resize t;
  if t.length = 0
  then set t 0 ~time ~bits ~mask
  else if time > get_time_at_index t (t.length - 1)
  then set t t.length ~time ~bits ~mask
  else (
    match find_insertion_index t time with
    | None ->
      shuffle_up t 0;
      set t 0 ~time ~bits ~mask
    | Some index ->
      (match Int.compare (get_time_at_index t index) time with
       | 0 -> set t index ~time ~bits ~mask
       | -1 ->
         shuffle_up t (index + 1);
         set t (index + 1) ~time ~bits ~mask
       | _ -> raise_s [%message "[insert] unhandled case"]))
;;

let insert_value t time value =
  insert t time ~bits:(Four_state_logic.bits value) ~mask:(Four_state_logic.mask value)
;;

(* [caller] is the entry point the user actually called ([get] or [get_mask]), so that the
   message attributes the failure correctly. *)
let[@cold] raise_invalid_time ~caller ~time =
  raise_s
    [%message
      ""
        ~_:
          ([%string "[Wave_data_in_mutable_events_four_state.%{caller}] Invalid time"]
           : string)
        (time : int)]
;;

let get_value (t : t) time ~caller ~value_at_index =
  (* If there are no events at all, return all-X (both values are all ones). Renderers ask
     about times in waves that never recorded an event, and must not crash; an event-less
     4-state wave reads as unknown rather than as a confident 0. *)
  if t.length = 0
  then Bits.ones t.width
  else (
    match find_insertion_index t time with
    | None -> raise_invalid_time ~caller ~time
    | Some index -> value_at_index t index)
;;

let get t time = get_value t time ~caller:"get" ~value_at_index:get_data_at_index

let get_mask t time =
  get_value t time ~caller:"get_mask" ~value_at_index:get_mask_at_index
;;

let get_digestible_string (t : t) =
  let bits, bits_length = Wave_data_in_cycles.get_digestible_string t.data.bits in
  let mask, mask_length = Wave_data_in_cycles.get_digestible_string t.data.mask in
  let total_length = 8 + 8 + (8 * t.length) + 8 + bits_length + mask_length in
  let bytes = Bytes.create total_length in
  let pos = ref 0 in
  let set_int64 x =
    assert (!pos + 8 <= total_length);
    Bytes.unsafe_set_int64 bytes !pos x;
    pos := !pos + 8
  in
  let set_int x = set_int64 (Int64.of_int x) in
  set_int t.width;
  set_int !(t.max_time);
  for i = 0 to t.length - 1 do
    set_int t.time.(i)
  done;
  (* Delimit the two values so their concatenation is unambiguous. *)
  set_int bits_length;
  Bytes.unsafe_blit ~src:bits ~src_pos:0 ~dst:bytes ~dst_pos:!pos ~len:bits_length;
  pos := !pos + bits_length;
  Bytes.unsafe_blit ~src:mask ~src_pos:0 ~dst:bytes ~dst_pos:!pos ~len:mask_length;
  pos := !pos + mask_length;
  assert (!pos = total_length);
  bytes, total_length
;;

module Serialization = struct
  module For_bin_io = struct
    type t_outer = t

    type t =
      { time : Delta_encoded_times.t
      ; bits : Wave_data_in_cycles.t
      ; mask : Wave_data_in_cycles.t
      ; length : int
      ; width : int
      ; max_time : int
      }
    [@@deriving bin_io]

    let of_t (t : t_outer) =
      { time = { Delta_encoded_times.used_length = t.length; data = t.time }
      ; bits = t.data.bits
      ; mask = t.data.mask
      ; length = t.length
      ; width = t.width
      ; max_time = !(t.max_time)
      }
    ;;

    let validate_value ~name ~length ~width value =
      if not (Int.equal width (Wave_data_in_cycles.width value))
      then
        raise_s
          [%message
            "Invalid serialized mutable-event data width"
              (name : string)
              (width : int)
              ~data_width:(Wave_data_in_cycles.width value : int)];
      if not (Int.equal length (Wave_data_in_cycles.length value))
      then
        raise_s
          [%message
            "Invalid serialized mutable-event data length"
              (name : string)
              (length : int)
              ~data_length:(Wave_data_in_cycles.length value : int)]
    ;;

    let to_t { time = { used_length; data = time }; bits; mask; length; width; max_time } =
      if width <= 0 || length < 0
      then
        raise_s
          [%message "Invalid serialized waveform dimensions" (width : int) (length : int)];
      if not (Int.equal used_length (Array.length time))
      then
        raise_s
          [%message
            "Invalid serialized time slice length"
              (used_length : int)
              ~time_length:(Array.length time : int)];
      if not (Int.equal length used_length)
      then
        raise_s
          [%message
            "Invalid serialized mutable-event length"
              (length : int)
              ~time_length:(used_length : int)];
      validate_value ~name:"bits" ~length ~width bits;
      validate_value ~name:"mask" ~length ~width mask;
      ({ time
       ; data = { bits; mask }
       ; length
       ; width
       ; max_time = ref max_time
       ; unique_id = new_unique_id ()
       }
       : t_outer)
    ;;
  end

  include
    Binable.Of_binable_with_uuid
      (For_bin_io)
      (struct
        type nonrec t = t

        let to_binable = For_bin_io.of_t
        let of_binable = For_bin_io.to_t

        let caller_identity =
          Bin_prot.Shape.Uuid.of_string "96ad5f29-0c49-4a9a-9ce9-f50488188768"
        ;;
      end)
end

include Serialization
