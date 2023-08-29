open! Base
include Pair_intf.T
module M = Pair_intf.M

module Wrap (Data : Interface.S) = struct
  module T = struct
    type nonrec 'a t = 'a Data.t t [@@deriving sexp_of]

    let port_names_and_widths =
      map port_names_and_widths ~f:(fun (p, _) ->
        Data.map Data.port_names_and_widths ~f:(fun (d, b) -> p ^ "_" ^ d, b))
    ;;

    let map t ~f = map t ~f:(Data.map ~f)
    let iter t ~f = iter t ~f:(Data.iter ~f)
    let map2 s t ~f = map2 s t ~f:(Data.map2 ~f)
    let iter2 s t ~f = iter2 s t ~f:(Data.iter2 ~f)
    let to_list t = List.concat (to_list t |> List.map ~f:Data.to_list)
  end

  include T
  include Interface.Make (T)
end
