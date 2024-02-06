open! Base

type 'a t =
  { read_clock : 'a
  ; read_address : 'a
  ; read_enable : 'a
  }
[@@deriving sexp_of]

let iter t ~f =
  f t.read_clock;
  f t.read_address;
  f t.read_enable
;;

let map t ~f =
  { read_clock = f t.read_clock
  ; read_address = f t.read_address
  ; read_enable = f t.read_enable
  }
;;

let zip s t =
  { read_clock = s.read_clock, t.read_clock
  ; read_address = s.read_address, t.read_address
  ; read_enable = s.read_enable, t.read_enable
  }
;;

let iter2 s t ~f = iter (zip s t) ~f:(fun (s, t) -> f s t)
let map2 s t ~f = map (zip s t) ~f:(fun (s, t) -> f s t)
let to_list t = [ t.read_clock; t.read_address; t.read_enable ]

let port_names =
  { read_clock = "read_clock"
  ; read_address = "read_address"
  ; read_enable = "read_enable"
  }
;;
