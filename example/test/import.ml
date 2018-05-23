include Base
include Hardcaml
include Hardcaml_examples
include Expect_test_helpers_kernel

let rec num_bits bits =
  if bits < 0 then raise_s [%message "arg to [num_bits] must be >= 0" (bits : int)];
  match bits with 0 | 1 -> 1 | _ -> 1 + (num_bits (bits / 2))

let concat = String.concat
let incr   = Int.incr

module Float2 = struct
  let string_of_float f =
    Float.round_significant ~significant_digits:2 f |> Float.to_string

  let sexp_of_float f = Sexp.Atom (f |> string_of_float)

  let sexp_of_log10 f =
    Sexp.Atom (concat [ "1E"; (Float.log10 f |> string_of_float) ])
end
