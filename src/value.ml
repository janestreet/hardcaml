open Base

module type Arg = Value_intf.Arg

module Make (S : sig
  val port_name : string
  val port_width : int
end) =
struct
  module T = struct
    type 'a t = 'a

    let sexp_of_t sexp_of_a a = [%sexp_of: string * a] (S.port_name, a)
    let port_names_and_widths = S.port_name, S.port_width
    let map t ~f = f t
    let iter t ~f = f t
    let map2 s t ~f = f s t
    let iter2 s t ~f = f s t
    let to_list t = [ t ]
  end

  include T
  include Interface.Make (T)
end
