open Base
open Hardcaml

type 'a signed_and_unsigned [@@deriving sexp_of]

val convert_bits
  :  ?f:(Signal.t -> Signal.t)
  -> uint:(Signal.t -> 'a)
  -> sint:(Signal.t -> 'a)
  -> trunc:(Signal.t -> 'a)
  -> int
  -> 'a Or_error.t signed_and_unsigned list

val pad_zero : int -> Signal.t -> Signal.t
