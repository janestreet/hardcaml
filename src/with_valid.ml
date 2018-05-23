open! Import

type 'a t =
  { valid : 'a
  ; value : 'a }
[@@deriving sexp_of]

let map x ~f = { valid = f x.valid; value = f x.value }
