open! Core0

module type Arg = sig
  val port_name : string
  val port_width : int
end

module type Arg_with_wave_format = sig
  include Arg

  val wave_format : Wave_format.t
end

module type S = Interface.S with type 'a t = 'a

module type Value = sig
  module type Arg = Arg
  module type Arg_with_wave_format = Arg_with_wave_format
  module type S = S

  module Make_with_wave_format (S : Arg_with_wave_format) : S
  module Make (S : Arg) : S

  val value : ?wave_format:Wave_format.t -> ?name:string -> int -> (module S)
end
