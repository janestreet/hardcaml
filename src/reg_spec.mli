type signal = Signal__type.t

type t = Signal__type.register =
  { reg_clock : signal
  ; reg_clock_edge : Edge.t
  ; reg_reset : signal
  ; reg_reset_edge : Edge.t
  ; reg_reset_value : signal
  ; reg_clear : signal
  ; reg_clear_level : Level.t
  ; reg_clear_value : signal
  ; reg_enable : signal
  }
[@@deriving sexp_of]

val create : ?clear:signal -> ?reset:signal -> unit -> clock:signal -> t

val override
  :  ?clock:signal
  -> ?clock_edge:Edge.t
  -> ?reset:signal
  -> ?reset_edge:Edge.t
  -> ?reset_to:signal
  -> ?clear:signal
  -> ?clear_level:Level.t
  -> ?clear_to:signal
  -> ?global_enable:signal
  -> t
  -> t

val reg_empty : t
val clock : t -> signal
val clear : t -> signal
val reset : t -> signal
