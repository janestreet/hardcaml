(** Implementation of runtime requirements for ppx_hardcaml. This module is not exported
    from this library. It exists only to allow ppx_hardcaml to be used within hardcaml
    itself. *)
open! Core0

module Array : sig
  include module type of Array

  val for_ : int -> f:(int -> unit) -> unit
end

module Int = Int
module Interface = Interface
module Wave_format = Wave_format
module List = List

val concat : ?sep:string -> string list -> string
