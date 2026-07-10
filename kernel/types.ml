open! Core0
module Clocking = Clocking
module Cross_product = Cross_product.Make
module Pair = Pair
module With_valid = With_valid

let value = Value.value

module type Value_arg = Value.Arg
module type Value = Value.S

module Value = Value.Make

let scalar = Scalar.scalar

module type Scalar = Scalar.S
module type Scalar_untyped = Scalar.S_untyped

module Scalar = Scalar.Make

module type Arg_with_length = sig
  include Value_arg

  val length : int
end

module type Interface_with_length = sig
  include Interface.Pre

  val length : int
end

module Interface_list (X : Interface_with_length) = struct
  module Pre = struct
    type 'a t = 'a X.t List.t [@@deriving equal ~localize, compare ~localize, sexp_of]

    let map a ~f = List.map a ~f:(local_ fun a -> X.map a ~f) [@nontail]
    let iter a ~f = List.iter a ~f:(local_ fun a -> X.iter a ~f) [@nontail]
    let map2 a b ~f = List.map2_exn a b ~f:(local_ fun a b -> X.map2 a b ~f) [@nontail]
    let iter2 a b ~f = List.iter2_exn a b ~f:(local_ fun a b -> X.iter2 a b ~f) [@nontail]
    let to_list = List.concat_map ~f:X.to_list

    let port_names_and_widths =
      List.init X.length ~f:(fun i ->
        X.map
          X.port_names_and_widths
          ~f:(Tuple2.map_fst ~f:(fun name -> [%string "%{name}_%{i#Int}"])))
    ;;
  end

  include Pre
  include Interface.Make (Pre)
end

module Interface_array (X : Interface_with_length) = struct
  module Pre = struct
    type 'a t = 'a X.t Array.t [@@deriving equal ~localize, compare ~localize, sexp_of]

    let map a ~f = Array.map a ~f:(local_ fun a -> X.map a ~f) [@nontail]
    let iter a ~f = Array.iter a ~f:(local_ fun a -> X.iter a ~f) [@nontail]
    let map2 a b ~f = Array.map2_exn a b ~f:(local_ fun a b -> X.map2 a b ~f) [@nontail]

    let iter2 a b ~f =
      Array.iter2_exn a b ~f:(local_ fun a b -> X.iter2 a b ~f) [@nontail]
    ;;

    let to_list a = Array.to_list a |> List.concat_map ~f:X.to_list

    let port_names_and_widths =
      Array.init X.length ~f:(fun i ->
        X.map
          X.port_names_and_widths
          ~f:(Tuple2.map_fst ~f:(fun name -> [%string "%{name}_%{i#Int}"])))
    ;;
  end

  include Pre
  include Interface.Make (Pre)
end

module List (X : Arg_with_length) = Interface_list (struct
    include X
    include Value (X)
  end)

module Array (X : Arg_with_length) = Interface_array (struct
    include X
    include Value (X)
  end)
