open! Import

type ('valid, 'value) t2 =
  { valid : 'valid
  ; value : 'value
  }
[@@deriving sexp_of]

type 'a t = ('a, 'a) t2 [@@deriving sexp_of]

let map x ~f = { valid = f x.valid; value = f x.value }
let map2 x y ~f = { valid = f x.valid y.valid; value = f x.value y.value }

let iter x ~f =
  f x.valid;
  f x.value
;;

let iter2 x y ~f =
  f x.valid y.valid;
  f x.value y.value
;;

let to_list { valid; value } = [ valid; value ]
