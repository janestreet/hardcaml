open! Core0

type data = Wave_data_in_cycles.t

(* Data sorted in time order. Times are stored unboxed in an [int array], while values are
   stored in [Wave_data_in_cycles] so they use the same packed representation as cycle
   traces. *)
type t = data Wave_data_in_mutable_events_kernel.t

include Wave_data_in_mutable_events_kernel.Shared_functions

let create width max_time = create ~create_data:Wave_data_in_cycles.create width max_time

let set_mutable_unsafe (t : t) index ~time data =
  t.time.(index) <- time;
  Wave_data_in_cycles.set_mutable_unsafe t.data index data;
  t.length <- max t.length (index + 1);
  t.max_time := max !(t.max_time) time
;;

let set (t : t) index ~time data =
  t.time.(index) <- time;
  Wave_data_in_cycles.set t.data index data;
  t.length <- max t.length (index + 1);
  t.max_time := max !(t.max_time) time
;;

let set_from_bytes (t : t) index ~time data byte_address =
  t.time.(index) <- time;
  Wave_data_in_cycles.set_from_bytes t.width t.data index data byte_address;
  t.length <- max t.length (index + 1);
  t.max_time := max !(t.max_time) time
;;

let%template[@mode m = (global, local)] get_data_at_index (t : t @ m) index =
  (Wave_data_in_cycles.get [@mode m]) t.data index
;;

(* Only called by renderers on 4-state data (see [Wave_data_intf.S]), so this is not a hot
   path and the allocation does not matter. *)
let get_mask_at_index (t : t) _ = Bits.zero t.width
let is_four_state _ = false

(* Content equality over the recorded events; unused capacity and cache state are ignored.
*)
let%template[@mode m = (local, global)] equal (a : t @ m) (b @ m) =
  let rec events_equal index =
    index = a.length
    || (Int.equal
          ((get_time_at_index [@mode m]) a index)
          ((get_time_at_index [@mode m]) b index)
        && Bits.equal
             ((get_data_at_index [@mode m]) a index)
             ((get_data_at_index [@mode m]) b index)
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
    f (get_time_at_index t index) (get_data_at_index t index)
  done
;;

let sexp_of_t (t : t) =
  let time = List.init t.length ~f:(get_time_at_index t) in
  let data = List.init t.length ~f:(get_data_at_index t) in
  let length = t.length in
  let width = t.width in
  let max_time = !(t.max_time) in
  [%sexp
    { time : int list; data : Bits.t list; length : int; width : int; max_time : int }]
;;

let shuffle_up (t : t) index =
  for i = t.length downto index + 1 do
    set t i ~time:(get_time_at_index t (i - 1)) (get_data_at_index t (i - 1))
  done
;;

let insert_at_end t time data = set t t.length ~time data

let insert_from_bytes_at_end t time data byte_address =
  set_from_bytes t t.length ~time data byte_address
;;

let insert (t : t) time data =
  if t.length = capacity t then resize t;
  if t.length = 0
  then set t 0 ~time data
  else if time > get_time_at_index t (t.length - 1)
  then insert_at_end t time data
  else (
    match find_insertion_index t time with
    | None ->
      shuffle_up t 0;
      set t 0 ~time data
    | Some index ->
      (match Int.compare (get_time_at_index t index) time with
       | 0 -> set t index ~time data
       | -1 ->
         shuffle_up t (index + 1);
         set t (index + 1) ~time data
       | _ -> raise_s [%message "[insert] unhandled case"]))
;;

let insert_from_bytes (t : t) time data byte_address =
  if t.length = capacity t then resize t;
  if t.length = 0
  then set_from_bytes t 0 ~time data byte_address
  else if time > get_time_at_index t (t.length - 1)
  then insert_from_bytes_at_end t time data byte_address
  else (
    match find_insertion_index t time with
    | None ->
      shuffle_up t 0;
      set_from_bytes t 0 ~time data byte_address
    | Some index ->
      (match Int.compare (get_time_at_index t index) time with
       | 0 -> set_from_bytes t index ~time data byte_address
       | -1 ->
         shuffle_up t (index + 1);
         set_from_bytes t (index + 1) ~time data byte_address
       | _ -> raise_s [%message "[insert_from_bytes] unhandled case"]))
;;

let insert_mutable_unsafe (t : t) time data =
  if t.length = capacity t then resize t;
  if t.length = 0
  then set_mutable_unsafe t 0 ~time data
  else if time > get_time_at_index t (t.length - 1)
  then set_mutable_unsafe t t.length ~time data
  else (
    match find_insertion_index t time with
    | None ->
      shuffle_up t 0;
      set_mutable_unsafe t 0 ~time data
    | Some index ->
      (match Int.compare (get_time_at_index t index) time with
       | 0 -> set_mutable_unsafe t index ~time data
       | -1 ->
         shuffle_up t (index + 1);
         set_mutable_unsafe t (index + 1) ~time data
       | _ -> raise_s [%message "[insert_mutable_unsafe] unhandled case"]))
;;

let get (t : t) time =
  (* If there are no events at all, just return 0 so we dont crash. *)
  if t.length = 0
  then Bits.zero t.width
  else (
    match find_insertion_index t time with
    | None ->
      raise_s [%message "[Wave_data_in_mutable_events.get] Invalid time" (time : int)]
    | Some index -> get_data_at_index t index)
;;

let get_digestible_string (t : t) =
  let data, data_length = Wave_data_in_cycles.get_digestible_string t.data in
  let total_length = 8 + 8 + (8 * t.length) + data_length in
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
  Bytes.unsafe_blit ~src:data ~src_pos:0 ~dst:bytes ~dst_pos:!pos ~len:data_length;
  pos := !pos + data_length;
  assert (!pos = total_length);
  bytes, total_length
;;

module Serialization = struct
  module For_bin_io = struct
    type t_outer = t

    type t =
      { time : Delta_encoded_times.t
      ; data : Wave_data_in_cycles.t
      ; length : int
      ; width : int
      ; max_time : int
      }
    [@@deriving bin_io]

    let of_t (t : t_outer) =
      { time = { used_length = t.length; data = t.time }
      ; data = t.data
      ; length = t.length
      ; width = t.width
      ; max_time = !(t.max_time)
      }
    ;;

    let to_t { time = { used_length; data = time }; data; length; width; max_time } =
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
      if not (Int.equal width (Wave_data_in_cycles.width data))
      then
        raise_s
          [%message
            "Invalid serialized mutable-event data width"
              (width : int)
              ~data_width:(Wave_data_in_cycles.width data : int)];
      if not (Int.equal length (Wave_data_in_cycles.length data))
      then
        raise_s
          [%message
            "Invalid serialized mutable-event data length"
              (length : int)
              ~data_length:(Wave_data_in_cycles.length data : int)];
      ({ time
       ; data
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
          Bin_prot.Shape.Uuid.of_string "fbb97829-49dc-4972-9b5d-c82122077125"
        ;;
      end)
end

include Serialization
