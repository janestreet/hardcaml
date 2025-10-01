open! Core0

module type Arg = sig
  val port_name : string
  val port_width : int
end

module type S = Interface.S with type 'a t = 'a

module type Value = sig
  module type Arg = Arg
  module type S = S

  module Make (S : Arg) : S

  val value : ?name:string -> int -> (module S)
end
