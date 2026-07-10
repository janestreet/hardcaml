open Core0

include struct
  open Wave_data_in_events_intf

  module type Data = Data
  module type Time = Time

  module M = M
end

module Make (Time : Time) (Data : Data) = struct
  (* Data sorted in time order. *)
  type t =
    { mutable data : Data.t array
    ; mutable time : Time.t array
    ; mutable length : int
    }
  [@@deriving sexp_of]

  let initial_size = 1

  let create () =
    { data = Array.create ~len:initial_size Data.none
    ; time = Array.create ~len:initial_size Time.zero
    ; length = 0
    }
  ;;

  (* Basic getter's and setter's *)

  let set t index time data =
    (* assume the array is large enough *)
    t.data.(index) <- data;
    t.time.(index) <- time;
    (* increase length if required *)
    t.length <- max t.length (index + 1)
  ;;

  let get_time_at_index t index = t.time.(index)
  let get_data_at_index t index = t.data.(index)
  let length t = t.length
  let capacity t = Array.length t.data

  let resize t =
    (* Double size and copy old data across *)
    let capacity = capacity t * 2 in
    let data = Array.create ~len:capacity Data.none in
    let time = Array.create ~len:capacity Time.zero in
    Array.blito ~src:t.data ~dst:data ~src_len:(length t) ();
    Array.blito ~src:t.time ~dst:time ~src_len:(length t) ();
    t.data <- data;
    t.time <- time
  ;;

  let find_insertion_index t time =
    (Array.binary_search
       ~len:(length t)
       t.time
       `Last_less_than_or_equal_to
       ~compare:Time.compare
       time
     |> [%globalize: int option])
    [@nontail]
  ;;

  let shuffle_up t index =
    for i = length t downto index + 1 do
      set t i (get_time_at_index t (i - 1)) (get_data_at_index t (i - 1))
    done
  ;;

  let insert t time data =
    (* Ensure we have space for a shuffle operation. *)
    if length t = capacity t then resize t;
    if length t = 0
    then (* This becomes our first element at index 0 *)
      set t 0 time data
    else (
      match find_insertion_index t time with
      | None ->
        (* Insert at 0, and shuffle everything up *)
        shuffle_up t 0;
        set t 0 time data
      | Some index ->
        (* 2 cases: time in array equals time of insert -> merge time in array is less
           than time of insert -> shuffle *)
        (match Time.compare (get_time_at_index t index) time with
         | 0 -> set t index time (Data.merge (get_data_at_index t index) data)
         | -1 ->
           shuffle_up t (index + 1);
           set t (index + 1) time data
         | _ -> raise_s [%message "[insert] unhandled case"]))
  ;;

  let get t time =
    match find_insertion_index t time with
    | None -> raise_s [%message "[Event_store.get] Invalid time" (time : Time.t)]
    | Some index -> get_data_at_index t index
  ;;
end

module Bits = struct
  module Data = struct
    type t = Bits.t [@@deriving sexp_of]

    let none = Bits.empty
    let merge _ x = x
  end

  module Time = struct
    type t = int [@@deriving compare ~localize, sexp_of]

    let zero = 0
  end

  module Event_store = Make (Time) (Data)

  type t =
    { t : Event_store.t
    ; width : int
    ; max_time : int ref
    }
  [@@deriving sexp_of]

  let create width max_time = { t = Event_store.create (); width; max_time }
  let length t = !(t.max_time) + 1

  let get t time =
    (* If there are no events at all, just return 0 so we dont crash. *)
    if Event_store.length t.t = 0 then Bits.zero t.width else Event_store.get t.t time
  ;;

  let%template equal _ _ = false [@@mode __ = (local, global)]
  let width t = t.width

  let get_digestible_string t =
    let num_events = Event_store.length t.t in
    let data_words = (t.width + 63) / 64 in
    let total_length = ref (8 + 8 + (8 * num_events) + (8 * data_words * num_events)) in
    let bytes = Bytes.create !total_length in
    let pos = ref 0 in
    let set_int64 x =
      assert (!pos + 8 <= !total_length);
      Bytes.unsafe_set_int64 bytes !pos x;
      pos := !pos + 8
    in
    let set_int x = set_int64 (Int64.of_int x) in
    let set_bits b =
      for i = 0 to data_words - 1 do
        set_int64 (Bits.unsafe_get_int64 b i)
      done
    in
    set_int t.width;
    set_int !(t.max_time);
    for i = 0 to num_events - 1 do
      set_int (Event_store.get_time_at_index t.t i);
      set_bits (Event_store.get_data_at_index t.t i)
    done;
    assert (!pos = !total_length);
    bytes, !total_length
  ;;

  let event_store t = t.t
end
