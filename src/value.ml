open! Core0

module type Arg = Value_intf.Arg
module type Arg_with_wave_format = Value_intf.Arg_with_wave_format
module type S = Value_intf.S

module Make_with_wave_format (S : Arg_with_wave_format) = struct
  module T = struct
    type 'a t = 'a [@@deriving equal ~localize, compare ~localize]

    let sexp_of_t sexp_of_a a = [%sexp_of: string * a] (S.port_name, a)
    let port_names_and_widths = S.port_name, S.port_width
    let map t ~f = f t
    let iter t ~f = f t
    let map2 s t ~f = f s t
    let iter2 s t ~f = f s t
    let to_list t = [ t ]
    let wave_formats = S.wave_format
  end

  include T
  include Interface.Make_with_wave_formats (T)
end

module Make (S : Arg) = Make_with_wave_format (struct
    include S

    let wave_format = Wave_format.default
  end)

let value ?wave_format ?(name = "value") width =
  let module M =
    Make_with_wave_format (struct
      let port_name = name
      let port_width = width
      let wave_format = Option.value wave_format ~default:Wave_format.default
    end)
  in
  (module M : S)
;;
