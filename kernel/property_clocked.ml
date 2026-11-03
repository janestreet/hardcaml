open! Core

type 'var t =
  { edge : Edge.t
  ; clock : 'var
  ; disable_iff : 'var option
  }
[@@deriving sexp_of]

let create ?disable_iff edge clock = { edge; clock; disable_iff }
