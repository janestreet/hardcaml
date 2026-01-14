open! Core

type 'a t =
  { before_edge : 'a
  ; after_edge : 'a
  }
[@@deriving sexp_of, fields ~getters]

let create ~before_edge ~after_edge = { before_edge; after_edge }
let const x = { before_edge = x; after_edge = x }
let map t ~f = { before_edge = f t.before_edge; after_edge = f t.after_edge }

let map2 a b ~f =
  { before_edge = f a.before_edge b.before_edge
  ; after_edge = f a.after_edge b.after_edge
  }
;;
