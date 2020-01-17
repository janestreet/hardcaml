(** Implementation of runtime requirements for ppx_deriving_hardcaml. This module is not
    exported from this library. It exists only to allow ppx_deriving_hardcaml to be used
    within hardcaml itself. *)
open Base

module Array : sig
  include module type of Array

  val for_ : int -> f:(int -> unit) -> unit
end

module Int = Int
module Interface = Interface
module List = List

val concat : ?sep:string -> string list -> string
