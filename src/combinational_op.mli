(** A custom combinational operation that can be inserted into a simulation. *)

open! Core0

(** Implementation of the custom operation using [Bits.Mutable.t] *)
type create_mutable_fn =
  inputs:Bits.Mutable.t list -> outputs:Bits.Mutable.t list -> (unit -> unit) Staged.t

(** Implementation of the custom operation using [Bits.t] *)
type create_fn = Bits.t list -> Bits.t list

type t [@@deriving sexp_of]

(** {2 Accessors} *)

val name : t -> string
val create_fn : t -> create_mutable_fn

(** {2 Api} *)

(** Create a combinational op providing both the input and output specification and an
    implementation. *)
val create
  :  name:string
  -> input_widths:int list
  -> output_widths:int list
  -> create_fn:create_fn
  -> unit
  -> t

(** Create a combinational op based on [Bits.Mutable.t] ports.

    This can be implemented in a zero alloc way, through the API is much harder to use. *)
val create_mutable
  :  name:string
  -> input_widths:int list
  -> output_widths:int list
  -> create_fn:create_mutable_fn
  -> unit
  -> t

(** Instantiate a combinational op within a hardcaml design. *)
val instantiate : t -> inputs:Signal.t list -> Signal.t list

(** Construct a combinational op from an input and output interface. *)
module With_interface (I : Interface.S) (O : Interface.S) : sig
  type create_fn = Bits.t I.t -> Bits.t O.t

  type create_mutable_fn =
    inputs:Bits.Mutable.t I.t -> outputs:Bits.Mutable.t O.t -> (unit -> unit) Staged.t

  val create : name:string -> create_fn:create_fn -> unit -> t
  val create_mutable : name:string -> create_fn:create_mutable_fn -> unit -> t
  val instantiate : t -> Interface.Create_fn(I)(O).t
end
