(** Random access memories described using RTL inference.

    Can be specified with arbitrary numbers of read and write ports, though in reality
    only up to 1 of each can be inferred by a synthesizer. *)

open! Import

module Collision_mode : sig
  type t =
    | Read_before_write
    | Write_before_read
  [@@deriving sexp_of, compare]

  include Comparable.S with type t := t
end

module Write_port : sig
  type t = Signal.write_port =
    { write_clock : Signal.t
    ; write_address : Signal.t
    ; write_enable : Signal.t
    ; write_data : Signal.t
    }
  [@@deriving sexp_of]
end

module Read_port : sig
  type t = Signal.read_port =
    { read_clock : Signal.t
    ; read_address : Signal.t
    ; read_enable : Signal.t
    }
  [@@deriving sexp_of]
end

val create
  :  collision_mode:Collision_mode.t
  -> size:int
  -> write_ports:Write_port.t array
  -> read_ports:Read_port.t array
  -> Signal.t array
