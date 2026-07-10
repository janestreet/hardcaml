(** Simple and fast distributed RAM based asynchronous FIFO. The depth of the FIFO is
    parameterizable, however, it should be less than or equal to [2 ^ LUT_SIZE] to avoid
    glitches on the addressing logic. *)

open Core0

module type S = sig
  (** Width of data in FIFO. *)
  val width : int

  (** Log2 of number of elements that can be stored in the FIFO. *)
  val log2_depth : int

  (** Optimize for the case where the reader is always reading and the clocks are in a
      similar rate.

      WARNING: This strips out any logic to pushback on the reader. *)
  val optimize_for_same_clock_rate_and_always_reading : bool
end

module Fifo_memory_type : sig
  type t =
    | Distributed
    | Registers
end

module Make (M : S) : sig
  module I : sig
    type 'a t =
      { clock_write : 'a
      ; clock_read : 'a
      ; reset_write : 'a
      ; reset_read : 'a
      ; data_in : 'a
      ; write_enable : 'a
      ; read_enable : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { full : 'a
      (** Note: the hardcaml async fifo can hold [2^log2_depth - 1] elements, one fewer
          than you might expect. *)
      ; almost_full : 'a
      (** [almost_full] is high if the fifo contains >= [capacity-2] elements. (i.e. 2
          writes away from being full). *)
      ; prog_full : 'a
      (** [prog_full] is high if the fifo contains >= [prog_full_thresh] elements. *)
      ; data_out : 'a
      ; valid : 'a
      ; almost_empty : 'a (** [almost_empty] is high if the fifo contains <= 2 elements *)
      ; prog_empty : 'a
      (** [prog_empty] is high if the fifo contains <= [prog_empty_thresh] elements *)
      }
    [@@deriving hardcaml]
  end

  val hierarchical
    :  ?name:string
    -> ?use_negedge_sync_chain:bool
    -> ?sync_stages:int
    -> ?memory_type:Fifo_memory_type.t
    -> ?prog_full_thresh:int
         (** [prog_full] is high if the fifo contains >= [prog_full_thresh] elements. When
             [None], [prog_full] is tied to [gnd]. *)
    -> ?prog_empty_thresh:int
         (** [prog_empty] is high if the fifo contains <= [prog_empty_thresh] elements.
             When [None], [prog_empty] is tied to [gnd]. *)
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  val create
    :  ?use_negedge_sync_chain:bool
         (** Whether to use the negative edge in the synchronization chain (default is
             false). *)
    -> ?sync_stages:int
         (** The number of synchronization stages to use for the gray coded registers
             (default is 2). *)
    -> ?memory_type:Fifo_memory_type.t (** Which style of ram to implement. *)
    -> ?prog_full_thresh:int
    -> ?prog_empty_thresh:int
    -> ?scope:Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  val create_clocked
    :  ?use_negedge_sync_chain:bool
         (** Whether to use the negative edge in the synchronization chain (default is
             false). *)
    -> ?sync_stages:int
         (** The number of synchronization stages to use for the gray coded registers
             (default is 2). *)
    -> ?memory_type:Fifo_memory_type.t (** Which style of ram to implement. *)
    -> ?prog_full_thresh:int
    -> ?prog_empty_thresh:int
    -> ?scope:Scope.t
    -> Clocked_signal.t I.t
    -> Clocked_signal.t O.t

  (** Create an async FIFO that [O.valid] goes high after [delay] clocks of a [o.valid]
      low start. This is useful for packet buffering across clock domains where you don't
      want the output [valid] to de-assert. *)
  val create_with_delay
    :  ?prog_full_thresh:int
    -> ?prog_empty_thresh:int
    -> ?delay:Int.t
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  val create_with_delay_clocked
    :  ?prog_full_thresh:int
    -> ?prog_empty_thresh:int
    -> ?delay:Int.t
    -> Scope.t
    -> Clocked_signal.t I.t
    -> Clocked_signal.t O.t

  val hierarchical_with_delay
    :  ?name:string
    -> ?prog_full_thresh:int
    -> ?prog_empty_thresh:int
    -> ?delay:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  module For_testing : sig
    (** Use a synchronous clear instead of an async reset for internal registers. This
        will not work when synthesizing to a multiclock design, but can be used to
        simulate the reset semantics in Cyclesim. *)
    val create_with_synchronous_clear_semantics_for_simulation_only
      :  ?use_negedge_sync_chain:bool
           (** Whether to use the negative edge in the synchronization chain (default is
               false). *)
      -> ?sync_stages:int
           (** The number of synchronization stages to use for the gray coded registers
               (default is 2). *)
      -> ?prog_full_thresh:int
      -> ?prog_empty_thresh:int
      -> ?scope:Scope.t
      -> Signal.t I.t
      -> Signal.t O.t
  end
end

module For_testing : sig
  val gray_inc_mux_inputs : (module Comb.S with type t = 'a) -> int -> by:int -> 'a list
end
