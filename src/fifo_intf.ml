open! Import

module type Config = sig
  val data_width : int
  val capacity : int
  val showahead : bool
end

module T = struct
  type 'a t =
    { q : 'a
    ; full : 'a
    ; empty : 'a
    ; nearly_full : 'a
    ; nearly_empty : 'a
    ; used : 'a
    }
  [@@deriving sexp_of, hardcaml]

  type 'a create_params =
    ?nearly_empty:int (** default is [1] **)
    -> ?nearly_full:int (** default is [depth-1] **)
    -> ?overflow_check:bool (** default is [true] *)
    -> ?reset:Signal.t (** default is [empty] **)
    -> ?underflow_check:bool (** default is [true] *)
    -> ?ram_attributes:Rtl_attribute.t list (** default is blockram *)
    -> ?scope:Scope.t (* to override naming prefix *)
    -> 'a

  type create_fifo =
    (unit
     -> capacity:int
     -> clock:Signal.t
     -> clear:Signal.t
     -> wr:Signal.t
     -> d:Signal.t
     -> rd:Signal.t
     -> Signal.t t)
      create_params
end

module Kinded_fifo = struct
  type ('a, 'b) t =
    | Classic : 'a T.t -> ('a, [ `Classic ]) t
    | Showahead : 'a T.t -> ('a, [ `Showahead ]) t

  type 'a packed = T : ('a, 'b) t -> 'a packed

  type create_classic =
    capacity:int
    -> write_clock:Signal.t
    -> read_clock:Signal.t
    -> clear:Signal.t
    -> wr:Signal.t
    -> d:Signal.t
    -> rd:Signal.t
    -> (Signal.t, [ `Classic ]) t

  type create_showahead =
    capacity:int
    -> write_clock:Signal.t
    -> read_clock:Signal.t
    -> clear:Signal.t
    -> wr:Signal.t
    -> d:Signal.t
    -> rd:Signal.t
    -> (Signal.t, [ `Showahead ]) t
end

module type S = sig
  include module type of T with type 'a t = 'a T.t
  module Kinded_fifo = Kinded_fifo

  (** {4 Base RTL FIFO} *)

  (** [create ~clock ~clear ~wr ~d ~rd capacity] builds a FIFO with [capacity] elements
      which is written with [d] when [wr] is high and read when [rd] is high.

      The default reset configuration is to use a synchronous [clr] signal.  An asynchronous
      [rst] may be optionally provided.  One of [clr] or [rst] must be non-empty.

      Optional overflow and underflow checking may be used.  Data will not be written(/read)
      when the fifo is [full](/[empty]) regardles or the [wr]/([rd]) signals.

      [nearly_emtpy] and [nearly_full] may be programmed to go high when the fifo is nearing
      an underflow or overflow state.

      The [showahead] mode changes the read behaviour of the FIFO. When showahead is [false]
      read data is available 1 cycle after [rd] is high. With showahead [true] the data is
      available on the same cycle as [rd] is high. To support [showahead] behaviour the
      timing of the [full]/[empty] flag also changes (although they still correctly indicate
      when it is safe to read or write to the FIFO). [showahead] mode has some extra cost in
      terms of extra logic. The implementation ensures the output is registered and timing
      performance is good - nearly as fast as the underlying RAM allows..

      Note; [showahead] is sometimes referred to as "first word fall through". It uses the
      write-before-read ram mode which is problematic in synthesis so we include special
      logic that performs collision detection.

      The [used] output indicates the number of elements currently in the FIFO. *)
  val create : ?showahead:bool (** default is [false] *) -> T.create_fifo


  (** {3 Functions to derive fifo architectures from other architecetures.} *)

  val showahead_fifo_of_classic_fifo
    :  Kinded_fifo.create_classic
    -> Kinded_fifo.create_showahead Staged.t

  (** {3 Derived FIFO architectures.} *)

  (** Adds an extra output register to the non-showahead fifo. This delays the output, but
      ensures there is no logic data on the fifo output. Adds an extra cycle of latency (2
      cycles from write to empty low).*)
  val create_classic_with_extra_reg : T.create_fifo

  (** Constructs a showahead fifo from a non-showahead fifo. Only modifies the control
      flags. Has 2 cycles of latency. *)
  val create_showahead_from_classic : create_fifo

  (** Constructs a fifo similarly to [create_showahead_from_classic] and ensures the output
      data is registered. Has 3 cycles of latency, but is slightly faster than [create
      ~showahead:true] - it seems to only be limited by the underlying RAM frequency. *)
  val create_showahead_with_extra_reg : create_fifo

  module type Config = Config

  (** Create FIFO using interfaces. *)
  module With_interface (Config : Config) : sig
    module I : sig
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; wr : 'a
        ; d : 'a
        ; rd : 'a
        }
      [@@deriving sexp_of, hardcaml]
    end

    module O : Interface.S with type 'a t = 'a t

    (** Create a normal or showahead fifo using different read-before-write or
        write-before-read (semantically) rams. *)
    val create : Signal.t Interface.Create_fn(I)(O).t create_params

    (** Create fifo using read-before-write ram only. It may still be used in showahead
        mode and include an extra register stage. Latency is slightly higher than the
        version built by [create]. *)
    val classic
      :  ?extra_reg:bool (* default is false *)
      -> Signal.t Interface.Create_fn(I)(O).t create_params
  end
end
