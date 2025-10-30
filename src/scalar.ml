open! Core0

module type S = Scalar_intf.S
module type S_untyped = Scalar_intf.S_untyped

module Make_with_wave_format (X : Value.Arg_with_wave_format) = struct
  include Value.Make_with_wave_format (X)

  let num_bits = X.port_width

  let check_width (type a) (module Comb : Comb.S with type t = a) ~expected_width bits =
    let got = Comb.width bits in
    if expected_width <> got
    then (
      let scalar_name = X.port_name in
      raise_s
        [%message
          "invalid bit width" (scalar_name : string) (expected_width : int) (got : int)])
  ;;

  let create (type a) (module Comb : Comb.S with type t = a) bits =
    check_width (module Comb) ~expected_width:port_widths bits;
    bits
  ;;

  let unwrap t = t

  module Unsafe = struct
    let wrap t = t
  end

  let apply f x = f x

  let to_with_valid (type a) (module Comb : Comb.S with type t = a) (x : a With_valid.t) =
    check_width (module Comb) ~expected_width:1 x.valid;
    check_width (module Comb) ~expected_width:port_widths x.value;
    x
  ;;

  let from_with_valid
    (type a)
    (module Comb : Comb.S with type t = a)
    (x : a With_valid.t t)
    =
    check_width (module Comb) ~expected_width:1 x.valid;
    check_width (module Comb) ~expected_width:port_widths x.value;
    x
  ;;

  let lift_with_valid t = t
  let lower_with_valid t = t
end

module Make (S : Value.Arg) = Make_with_wave_format (struct
    include S

    let wave_format = Wave_format.default
  end)

let scalar ?wave_format ?(name = "scalar") port_width =
  let module M =
    Make_with_wave_format (struct
      let port_name = name
      let port_width = port_width
      let wave_format = Option.value wave_format ~default:Wave_format.default
    end)
  in
  (module M : S_untyped)
;;
