open Base

module type S = Scalar_intf.S
module type S_untyped = Scalar_intf.S_untyped

module Make (X : Value.Arg) = struct
  include Value.Make (X)

  let num_bits = X.port_width

  let create (type a) (module Comb : Comb.S with type t = a) bits =
    let expected_width = port_widths in
    let got = Comb.width bits in
    if expected_width <> got
    then (
      let scalar_name = X.port_name in
      raise_s
        [%message
          "invalid bit width" (scalar_name : string) (expected_width : int) (got : int)]);
    bits
  ;;

  let unwrap t = t

  module Unsafe = struct
    let wrap t = t
  end

  let apply f x = f x
end
