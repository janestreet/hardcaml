include Base
include Hardcaml
include Hardcaml_examples

let rec num_bits bits =
  if bits < 0 then raise_s [%message "arg to [num_bits] must be >= 0" (bits : int)];
  match bits with 0 | 1 -> 1 | _ -> 1 + (num_bits (bits / 2))
