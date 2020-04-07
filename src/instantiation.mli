(** Instantiation of sub-modules.

    For use with generated RTL when we want to reference some non-hardcaml modules. *)

open! Import

type instobj = < i : string -> Signal.t ; o : string -> Signal.t >

val create
  :  ?lib:string
  -> ?arch:string
  -> ?instance:string
  -> ?parameters:Parameter.t list
  -> unit
  -> name:string
  -> inputs:(string * Signal.t) list
  -> outputs:(string * int) list
  -> instobj

module With_interface (I : Interface.S) (O : Interface.S) : sig
  (** Instantiate an RTL design with the given input and output interface. *)
  val create
    :  ?lib:string
    -> ?arch:string
    -> ?instance:string
    -> ?parameters:Parameter.t list
    -> name:string
    -> Signal.t Interface.Create_fn(I)(O).t
end
