(** Synchronous FIFO. *)

open! Import

type t =
  { q            : Signal.t
  ; full         : Signal.t
  ; empty        : Signal.t
  ; nearly_full  : Signal.t
  ; nearly_empty : Signal.t
  ; used         : Signal.t }
[@@deriving sexp_of]

(** [create ~clk ~clr ~wr ~d ~rd capacity] builds a FIFO with [capacity] elements which is
    written with [d] when [wr] is high and read when [rd] is high.

    The default reset configuration is to use a synchronous [clr] signal.  An asynchronous
    [rst] may be optionally provided.  One of [clr] or [rst] must be non-empty.

    Optional overflow and underflow checking may be used.  Data will not be written(/read)
    when the fifo is [full](/[empty]) regardles or the [wr]/([rd]) signals.

    [nearly_emtpy] and [nearly_full] may be programmed to go high when the fifo is nearing
    an underflow or overflow state.

    The [showahead] mode changes the read behaviour of the FIFO.  When showahead is
    [false] read data is available 1 cycle after [rd] is high.  With showahead [true] the
    data is available on the same cycle as [rd] is high.  To support [showahead] behaviour
    the timing of the [full]/[empty] flag also changes (although they still correctly
    indicate when it is safe to read or write to the FIFO).  [showahead] mode has some
    extra cost in terms of extra logic and reduced frequency.

    Note; [showahead] is sometimes referred to as "first word fall through".

    The [used] output indicates the number of elements currently in the FIFO. *)
val create
  :  ?nearly_empty    : int       (** default is [1] **)
  -> ?nearly_full     : int       (** default is [depth-1] **)
  -> ?overflow_check  : bool      (** default is [true] *)
  -> ?reset           : Signal.t  (** default is [empty] **)
  -> ?showahead       : bool      (** default is [false] **)
  -> ?underflow_check : bool      (** default is [true] *)
  -> unit
  -> capacity : int
  -> clock : Signal.t
  -> clear : Signal.t
  -> wr    : Signal.t
  -> d     : Signal.t
  -> rd    : Signal.t
  -> t

