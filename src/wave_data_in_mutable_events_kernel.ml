open! Core0

type 'data t =
  { mutable time : int array
  ; data : 'data
  ; mutable length : int
  ; width : int
  ; max_time : int ref
  ; unique_id : int
  }

module Shared_functions = struct
  let new_unique_id =
    let counter = ref 0 in
    fun () ->
      Int.incr counter;
      !counter
  ;;

  let unique_id t = t.unique_id
  let initial_size = 1

  let create ~create_data width max_time =
    { time = Array.create ~len:initial_size 0
    ; data = create_data width
    ; length = 0
    ; width
    ; max_time
    ; unique_id = new_unique_id ()
    }
  ;;

  let width t = t.width
  let length t = !(t.max_time) + 1
  let num_events t = t.length
  let capacity t = Array.length t.time

  (* Grow to at least [initial_size]: a [t] deserialized from a payload with no events has
     capacity 0, and doubling that would leave no room for the next insert. *)
  let resize t =
    let capacity = max initial_size (capacity t * 2) in
    let time = Array.create ~len:capacity 0 in
    Array.blito ~src:t.time ~dst:time ~src_len:t.length ();
    t.time <- time
  ;;

  let get_time_at_index t index = t.time.(index)

  let find_insertion_index t time =
    (Array.binary_search
       ~len:t.length
       t.time
       `Last_less_than_or_equal_to
       ~compare:Int.compare
       time
     |> [%globalize: int option])
    [@nontail]
  ;;
end
