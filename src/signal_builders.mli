open! Core0

(** Functors to construct methods needed for a [Signal.S] interface to be completed. *)

(** Construct a constant propagating version of [Comb.S] implementation that will fold
    over compile-time constants. *)
module Const_prop (Unoptimized : sig
    include Comb.S

    val is_const : t -> bool
    val const_value : t -> Bits.t
  end) : Comb.S with type t = Unoptimized.t

(** Construct conversion functions from signals to other types *)
module Conversion_functions (Comb : Comb.S) : sig
  open Comb

  val of_bits : Bits.t -> t
  val to_bits : t -> Bits.t
end

(** Construct memory functions for a [Signal.S] interface. *)
module Memories (Comb : sig
    include Comb.S
    module Reg_spec : Reg_spec.S with type signal := t

    val reg
      :  ?enable:t
      -> ?initialize_to:Bits.t
      -> ?reset_to:Bits.t
      -> ?clear:t
      -> ?clear_to:t
      -> Reg_spec.t
      -> t
      -> t

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

  val memory
    :  ?attributes:Rtl_attribute.t list
    -> int
    -> write_port:t Write_port.t
    -> read_address:t
    -> t

  val ram_wbr
    :  ?name:string
    -> ?attributes:Rtl_attribute.t list
    -> write_port:t Write_port.t
    -> read_port:t Read_port.t
    -> int
    -> t

  val ram_rbw
    :  ?name:string
    -> ?attributes:Rtl_attribute.t list
    -> write_port:t Write_port.t
    -> read_port:t Read_port.t
    -> int
    -> t
end

(** Construct methods for building registered values. *)
module Registers (Comb : sig
    include Comb.S

    type info

    val add_attribute : t -> Rtl_attribute.t -> t

    module Reg_spec : Reg_spec.S with type signal := t

    val reg__with_signal_reset
      :  ?enable:t
      -> ?initialize_to:Bits.t
      -> ?reset_to:t
      -> ?clear:t
      -> ?clear_to:t
      -> Reg_spec.t
      -> t
      -> t

    val wire : int -> t
    val assign : t -> t -> unit
    val to_rep : t -> Signal__type.t * info
    val update_rep : t -> info:info -> t
  end) : sig
  open Comb

  val reg
    :  ?enable:t
    -> ?initialize_to:Bits.t
    -> ?reset_to:Bits.t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> t
    -> t

  val reg_fb
    :  ?enable:t
    -> ?initialize_to:Bits.t
    -> ?reset_to:Bits.t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> width:int
    -> f:(t -> t)
    -> t

  (** Pipeline a signal [n] times with the given register specification. If set, a list of
      RTL attributes will also be applied to each register created. *)
  val pipeline
    :  ?attributes:Rtl_attribute.t list
    -> ?enable:t
    -> ?initialize_to:Bits.t
    -> ?reset_to:Bits.t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> n:int
    -> t
    -> t

  (** [Staged.unstage (prev spec ?enable d)] returns a function [prev n] which provides
      [d] registered [n] times (ie the value of [d] [n] cycles in the past). [n=0] means
      the current (combinational value).

      The internal registers are shared between calls. When called multiple times with a
      maximum value of [n] exactly [n] registers are created. *)
  val prev
    :  ?enable:t
    -> ?initialize_to:Bits.t
    -> ?reset_to:Bits.t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> (t -> (int -> t) Staged.t)

  (** A register which forwards its input value to the output during cycles in which it is
      enabled.

      Implemented as a register with a mux on the output. Note that the cut through
      bevahiour will also occur if enable is high during a clear (or even reset)
      operation. *)
  val cut_through_reg
    :  ?initialize_to:Bits.t
    -> ?reset_to:Bits.t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> enable:t
    -> t
    -> t

  (** Basic counter. Adds [by] on each [enabled] cycle. Wraps on over/underflow. *)
  val counter
    :  ?enable:t
    -> ?initialize_to:Bits.t
    -> ?reset_to:Bits.t
    -> ?clear:t
    -> ?clear_to:t
    -> ?by:int (** Default is [1] *)
    -> Reg_spec.t
    -> width:int
    -> t
end
