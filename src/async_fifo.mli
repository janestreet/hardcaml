(** Simple and fast distributed RAM based asynchronous FIFO. The depth of the FIFO is
    parameterizable, however, it should be less than or equal to [2 ^ LUT_SIZE] to avoid
    glitches on the addressing logic. *)

module type S = sig
  (** Width of data in FIFO. *)
  val width : int

  (** Log2 of number of elements that can be stored in the FIFO. *)
  val log2_depth : int
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
      ; data_out : 'a
      ; valid : 'a
      ; almost_empty : 'a
      }
    [@@deriving hardcaml]
  end

  val hierarchical
    :  ?name:string
    -> ?use_negedge_sync_chain:bool
    -> ?sync_stages:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  val create
    :  ?use_negedge_sync_chain:bool
         (** Whether to use the negative edge in the synchronization chain (default is
        false). *)
    -> ?sync_stages:int
         (** The number of synchronization stages to use for the gray coded registers (default
        is 2). *)
    -> ?scope:Scope.t
    -> Signal.t I.t
    -> Signal.t O.t

  (** Create an async FIFO that [O.valid] goes high after [delay] clocks of a [o.valid]
      low start. This is useful for packet buffering across clock domains where you don't
      want the output [valid] to de-assert.*)
  val create_with_delay : ?delay:Int.t -> Scope.t -> Signal.t I.t -> Signal.t O.t

  val hierarchical_with_delay
    :  ?name:string
    -> ?delay:int
    -> Scope.t
    -> Signal.t I.t
    -> Signal.t O.t
end
