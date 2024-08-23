type signal = Signal__type.t

type t = Signal__type.reg_spec =
  { clock : signal
  ; clock_edge : Edge.t
  ; reset : signal
  ; reset_edge : Edge.t
  ; clear : signal
  }
[@@deriving sexp_of]

val create
  :  ?clock_edge:Edge.t
  -> ?reset:signal
  -> ?reset_edge:Edge.t
  -> ?clear:signal
  -> unit
  -> clock:signal
  -> t

val override
  :  ?clock:signal
  -> ?clock_edge:Edge.t
  -> ?reset:signal
  -> ?reset_edge:Edge.t
  -> ?clear:signal
  -> t
  -> t

val clock : t -> signal
val clock_edge : t -> Edge.t
val reset : t -> signal
val reset_edge : t -> Edge.t
val clear : t -> signal
