open! Base
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

module List (X : Arg_with_length) = struct
  module Pre = struct
    include Base.List

    let map2 = map2_exn
    let iter2 = iter2_exn

    let port_names_and_widths =
      init X.length ~f:(fun i -> [%string "%{X.port_name}%{i#Int}"], X.port_width)
    ;;
  end

  include Pre
  include Interface.Make (Pre)
end

module Array (X : Arg_with_length) = struct
  module Pre = struct
    include Base.Array

    let map2 = map2_exn
    let iter2 = iter2_exn

    let port_names_and_widths =
      init X.length ~f:(fun i -> [%string "%{X.port_name}%{i#Int}"], X.port_width)
    ;;
  end

  include Pre
  include Interface.Make (Pre)
end
