open! Import

include Fixnum_intf

module Make (Spec : Spec) = struct
  type t = int

  let width = Spec.width

  let to_scaled_int t = t

  let round x = Float.to_int (if Float.(x < 0.) then x -. 0.5 else x +. 0.5)

  let scale = 2. **. Float.of_int Spec.fractional_width

  let of_float x = round (x *. scale)

  let to_float t = Float.of_int t /. scale

  let of_bits bits = bits |> Bits.to_sint

  let to_bits t = Bits.consti width t

  let constb t = t |> to_bits |> Bits.to_bstr

  let pow2 i = Int.( lsl ) 1 (Spec.fractional_width + i)

  let bits_constf f = f |> of_float |> to_bits

  let signal_mul a b =
    Signal.(select (sra (a *+ b) Spec.fractional_width) (Spec.width-1) 0)

  let signal_constf f = Signal.consti width (f |> of_float)
end
