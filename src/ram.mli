(** Random access memories described using RTL inference.

    Can be specified with arbitrary numbers of read and write ports, though in reality
    only up to 1 of each can be inferred by a synthesizer. *)

open! Core0

module Collision_mode : sig
  type t =
    | Read_before_write
    | Write_before_read
  [@@deriving sexp_of, compare ~localize]

  include%template Comparable.S_plain [@mode local] with type t := t
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

(** A dual port ram which can be inferred from RTL.

    The structure is to describe 2 ports each of which can read or write. This is a little
    different to the hardcaml default which seperates reads and writes, but much closer
    matches the way hardware rams are actually architected.

    It implements [No_change] address collision mode, as this is the only commonly
    supported mode across xilinx ram types (especially UltraRAM).

    Note: We must support hierarchical instantiation of the memory - inference is very
    picky if you muck around with the address for example, but works fine if it's a
    sub-module. *)
module Dual_port : sig
  module type Config = sig
    val address_bits : int
    val data_bits : int
  end

  module Make (Config : Config) : sig
    module Port : sig
      type 'a t =
        { address : 'a
        ; data : 'a
        ; enable : 'a
        ; write : 'a
        }
      [@@deriving hardcaml]
    end

    module I : sig
      type 'a t =
        { clock : 'a
        ; port_a : 'a Port.t
        ; port_b : 'a Port.t
        }
      [@@deriving hardcaml]
    end

    module O : sig
      type 'a t =
        { qa : 'a
        ; qb : 'a
        }
      [@@deriving hardcaml]
    end

    val create
      :  ?attributes:Rtl_attribute.t list (* attributes applied to the memory array *)
      -> ?name:string (* name of memory array *)
      -> ?size:int (* size of the memory (if not [1 << address_bits]) *)
      -> Scope.t
      -> Interface.Create_fn(I)(O).t

    val hierarchical
      :  ?attributes:Rtl_attribute.t list (* attributes applied to the instantiation *)
      -> ?name:string (* name of module *)
      -> ?instance:string (* instance name *)
      -> ?memory_attributes:Rtl_attribute.t list
           (* attributes applied to the memory array *)
      -> ?memory_name:string (* name of memory array *)
      -> ?size:int (* size of the memory (if not [1 << address_bits]) *)
      -> Scope.t
      -> Interface.Create_fn(I)(O).t
  end
end
