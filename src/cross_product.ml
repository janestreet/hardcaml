open! Core0

module type S = Cross_product_intf.S

module Make (Outer : Interface.S) (Inner : Interface.S) = struct
  module type S = Interface.S with type 'a t = 'a Inner.t Outer.t

  module Pre = struct
    type 'a t = 'a Inner.t Outer.t
    [@@deriving equal ~localize, compare ~localize, sexp_of]

    let map t ~f = Outer.map t ~f:(Inner.map ~f) [@nontail]
    let iter t ~f = Outer.iter t ~f:(Inner.iter ~f) [@nontail]
    let to_list t = List.concat_map ~f:Inner.to_list (Outer.to_list t)

    let port_names_and_widths =
      Outer.map Outer.port_names_and_widths ~f:(fun (n, _) ->
        Inner.map Inner.port_names_and_widths ~f:(fun (m, b) -> n ^ "_" ^ m, b))
    ;;

    let map2 a b ~f = Outer.map2 a b ~f:(Inner.map2 ~f) [@nontail]
    let iter2 a b ~f = Outer.iter2 a b ~f:(Inner.iter2 ~f) [@nontail]
  end

  include Pre
  include Interface.Make (Pre)

  let wave_formats = Outer.map Outer.wave_formats ~f:(fun _ -> Inner.wave_formats)
end
