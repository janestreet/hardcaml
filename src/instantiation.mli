(** Instantiation of sub-modules.

    For use with generated RTL when we want to reference some non-hardcaml modules. *)

open! Core0

type t
type output_map = Signal.t Map.M(String).t

val create
  :  ?lib:string
  -> ?arch:string
  -> ?instance:string
  -> ?parameters:Parameter.t list
  -> ?attributes:Rtl_attribute.t list
  -> unit
  -> name:string
  -> inputs:(string * Signal.t) list
  -> outputs:(string * int) list
  -> t

(** Return the underlying [Inst] signal constructor. *)
val instantiation_signal : t -> Signal.t

(** Return a map from output port name to output signal. *)
val outputs : t -> output_map

(** Find an output signal from it's name. Raises if it doesns't exist. *)
val output : t -> string -> Signal.t

module With_interface (I : Interface.S) (O : Interface.S) : sig
  (** Instantiate an RTL design with the given input and output interface. *)
  val create
    :  ?lib:string
    -> ?arch:string
    -> ?instance:string
    -> ?parameters:Parameter.t list
    -> ?attributes:Rtl_attribute.t list
    -> name:string
    -> Interface.Create_fn(I)(O).t
end

module Expert : sig
  (* Check that name only contains alphanumeric or underscore characters. *)
  val validate_module_name : string -> unit
end
