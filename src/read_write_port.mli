open! Core0

module Write_port : sig
  include Write_port.S with type 'a t = 'a Write_port.t

  module type Config = sig
    val address_bits : int
    val data_bits : int
    val enable_bits : int
  end

  module Make (Config : Config) : Interface.S with type 'a t = 'a Write_port.t
end

module Read_port : sig
  include Read_port.S with type 'a t = 'a Read_port.t

  module type Config = sig
    val address_bits : int
  end

  module Make (Config : Config) : Interface.S with type 'a t = 'a Read_port.t
end
