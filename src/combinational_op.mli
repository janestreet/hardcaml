(** A custom combinational operation that can be inserted into a simulation. *)

open Base

(** Implementation of the custom operation using [Bits.Mutable.t] *)
type create_fn = Bits.Mutable.t list -> Bits.Mutable.t list -> unit

type t [@@deriving sexp_of]

val create
  :  name:string
  -> input_widths:int list
  -> output_widths:int list
  -> create_fn:create_fn
  -> unit
  -> t

(** Constructed a [create_fn] from a [Bits.t] implementation. This will allocate and be a
    little less efficient, but might be simpler to write. *)
val create_fn_of_bits : (Bits.t list -> Bits.t list) -> create_fn

val name : t -> string
val create_fn : t -> create_fn

(** Instantiate a custom operation within a hardcaml design. *)
val instantiate : t -> inputs:Signal.t list -> Signal.t list
