(** Random access memories described using RTL inference.

    Can be specified with arbitrary numbers of read and write ports, though in reality
    only up to 1 of each can be inferred by a synthesizer. *)

open Base

module Collision_mode : sig
  type t =
    | Read_before_write
    | Write_before_read
  [@@deriving sexp_of, compare]

  include Comparable.S with type t := t
end

val create
  :  ?attributes:Rtl_attribute.t list
  -> ?name:string
  -> collision_mode:Collision_mode.t
  -> size:int
  -> write_ports:Signal.t Write_port.t array
  -> read_ports:Signal.t Read_port.t array
  -> unit
  -> Signal.t array
