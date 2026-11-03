open! Core0

type 'data t =
  { mutable time : int array
  ; data : 'data
  ; mutable length : int
  ; width : int
  ; max_time : int ref
  ; unique_id : int
  }

module Shared_functions : sig
  val new_unique_id : unit -> int
  val unique_id : _ t -> int
  val create : create_data:(int -> 'a) -> int -> int ref -> 'a t
  val width : _ t -> int
  val length : _ t -> int
  val capacity : _ t -> int
  val num_events : _ t -> int
  val resize : _ t -> unit
  val get_time_at_index : _ t -> int -> int
  val find_insertion_index : _ t -> int -> int option
end
