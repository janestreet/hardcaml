include Base
include Hardcaml

let print_endline = Stdio.print_endline

let print_s sexp = print_endline (Sexp.to_string_hum sexp)

let rec num_bits bits =
  if bits < 0 then raise_s [%message "arg to [num_bits] must be >= 0" (bits : int)];
  match bits with
  | 0 | 1 -> 1
  | _ -> 1 + num_bits (bits / 2)

let clog2 bits =
  if bits < 0 then raise_s [%message "arg to [clog2] must be >= 0" (bits : int)];
  match bits with
  | 0
  | 1 -> 1
  | _ -> Int.ceil_log2 bits
