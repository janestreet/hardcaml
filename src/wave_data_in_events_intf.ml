open! Core0

module type Data = sig
  type t [@@deriving sexp_of]

  val none : t
  val merge : t -> t -> t
end

module type Time = sig
  type t [@@deriving compare ~localize, sexp_of]

  val zero : t
end

module M (Time : Time) (Data : Data) = struct
  module type S = sig
    type t [@@deriving sexp_of]

    val create : unit -> t
    val set : t -> int -> Time.t -> Data.t -> unit
    val get_time_at_index : t -> int -> Time.t
    val get_data_at_index : t -> int -> Data.t
    val length : t -> int
    val capacity : t -> int
    val resize : t -> unit
    val find_insertion_index : t -> Time.t -> int option
    val insert : t -> Time.t -> Data.t -> unit
    val get : t -> Time.t -> Data.t
  end
end

module type Wave_data_in_events = sig
  module type Data = Data
  module type Time = Time

  module M = M
  module Make (Time : Time) (Data : Data) : M(Time)(Data).S

  module Bits : sig
    module Time : Time with type t = int
    module Data : Data with type t = Bits.t
    module Event_store : M(Time)(Data).S

    type t [@@deriving sexp_of, equal ~localize]

    val create : int -> int ref -> t
    val event_store : t -> Event_store.t

    (** Data.S interface *)

    val width : t -> int
    val length : t -> int
    val get : t -> int -> Bits.t
    val get_digestible_string : t -> Bytes.t * int
  end
end
