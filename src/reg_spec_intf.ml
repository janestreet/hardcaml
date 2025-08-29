(** Clock, reset and clear specification for registers. *)

open! Core0

module type Signal = sig
  type t [@@deriving sexp_of]

  val is_empty : t -> bool
end

module type S = sig
  type t [@@deriving sexp_of]
  type signal [@@deriving sexp_of]

  (** Create a [Reg_spec.t]. You must at a minimum provide a clock. [clear] and [reset]
      are optional. The [clock_edge] and [reset_edge] default to [Rising]. *)
  val create
    :  ?clock_edge:Edge.t
    -> ?reset:signal
    -> ?reset_edge:Edge.t
    -> ?clear:signal
    -> unit
    -> clock:signal
    -> t

  (** Override one or more fields of an existing [Reg_spec.t]. *)
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
  val reset : t -> signal option
  val reset_exn : t -> signal
  val reset_edge : t -> Edge.t
  val clear : t -> signal option
  val clear_exn : t -> signal
end

module type Reg_spec = sig
  module type Signal = Signal
  module type S = S

  module Make (Signal : Signal) : S with type signal := Signal.t
end
