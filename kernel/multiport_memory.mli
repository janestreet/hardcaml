(** Internal module - functions for implementing multiport memories and validating their
    parameters. *)

open! Core0

module Make (Comb : sig
    include Comb.S

    val multiport_memory_prim
      :  ?name:string
      -> ?attributes:Rtl_attribute.t list
      -> ?initialize_to:Bits.t array
      -> int
      -> remove_unused_write_ports:bool
      -> data_width:int
      -> write_ports:t Write_port.t array
      -> read_addresses:t array
      -> t array
  end) : sig
  open Comb

  val multiport_memory
    :  ?enable_modelling_features:bool
    -> ?verbose:bool
    -> ?name:string
    -> ?attributes:Rtl_attribute.t list
    -> ?initialize_to:Bits.t array
    -> int
    -> write_ports:t Write_port.t array
    -> read_addresses:t array
    -> t array

  val rom : read_addresses:t array -> Bits.t array -> t array
end
