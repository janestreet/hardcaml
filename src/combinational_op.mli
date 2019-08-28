(** A custom combinational operation that can be inserted into a simulation.

    A [create_fn] is required for [Cyclesim.create_functional] and a [create_fn_mutable]
    for [Cyclesim.create_imperative].  Only one needs to be provided and the other will be
    automatically derived.  For efficiency's sake, prefer [create_fn_mutable] and
    [Cyclsim.create_imperative]. *)

open! Import

(** Implementation of the custom operation using [Bits.t] *)
type create_fn = Bits.t list -> Bits.t list

(** Implementation of the custom operation using [Bits.Mutable.t] *)
type create_fn_mutable = Bits.Mutable.t list -> Bits.Mutable.t list -> unit

type t [@@deriving sexp_of]

(** One must supply at least one of [create_fn] and [create_fn_mutable]. *)
val create
  :  ?create_fn:create_fn
  -> ?create_fn_mutable:create_fn_mutable
  -> unit
  -> name:string
  -> input_widths:int list
  -> output_widths:int list
  -> t

val name : t -> string
val create_fn : t -> create_fn
val create_fn_mutable : t -> create_fn_mutable

(** Instantiate a custom operation within a hardcaml design. *)
val instantiate : t -> inputs:Signal.t list -> Signal.t list
