open Base

module type Arg = sig
  val port_name : string
  val port_width : int
end

module type Value = sig
  module type Arg = Arg

  module Make (S : Arg) : Interface.S with type 'a t = 'a
end
