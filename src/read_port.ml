open! Core0

module type S = Read_port_intf.S

type 'a t =
  { read_clock : 'a
  ; read_address : 'a
  ; read_enable : 'a
  }
[@@deriving bin_io, sexp_of, equal ~localize, compare ~localize]

let iter t ~(f @ local) =
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

let iter2 s t ~(f @ local) = iter (zip s t) ~f:(fun (s, t) -> f s t) [@nontail]
let map2 s t ~f = map (zip s t) ~f:(fun (s, t) -> f s t) [@nontail]
let to_list t = [ t.read_clock; t.read_address; t.read_enable ]

let port_names =
  { read_clock = "read_clock"
  ; read_address = "read_address"
  ; read_enable = "read_enable"
  }
;;
