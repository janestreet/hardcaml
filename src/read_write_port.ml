open! Core0

module Write_port = struct
  include Write_port

  module type Config = sig
    val address_bits : int
    val data_bits : int
    val enable_bits : int
  end

  module Make (Config : Config) = struct
    let port_names_and_widths =
      Write_port.map2
        Write_port.port_names
        { write_clock = 1
        ; write_address = Config.address_bits
        ; write_enable = Config.enable_bits
        ; write_data = Config.data_bits
        }
        ~f:(fun a b -> a, b)
    ;;

    include
      Interface.Update
        (struct
          include Write_port

          let port_names_and_widths = port_names_and_widths
        end)
        (struct
          let port_names_and_widths = port_names_and_widths
        end)
  end
end

module Read_port = struct
  include Read_port

  module type Config = sig
    val address_bits : int
  end

  module Make (Config : Config) = struct
    let port_names_and_widths =
      Read_port.map2
        Read_port.port_names
        { read_clock = 1; read_address = Config.address_bits; read_enable = 1 }
        ~f:(fun a b -> a, b)
    ;;

    include
      Interface.Update
        (struct
          include Read_port

          let port_names_and_widths = port_names_and_widths
        end)
        (struct
          let port_names_and_widths = port_names_and_widths
        end)
  end
end
