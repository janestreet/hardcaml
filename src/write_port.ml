open! Core0

type 'a t =
  { write_clock : 'a
  ; write_address : 'a
  ; write_enable : 'a
  ; write_data : 'a
  }
[@@deriving bin_io, sexp_of]

let iter t ~f =
  f t.write_clock;
  f t.write_address;
  f t.write_enable;
  f t.write_data
;;

let map t ~f =
  let write_clock = f t.write_clock in
  let write_address = f t.write_address in
  let write_data = f t.write_data in
  let write_enable = f t.write_enable in
  { write_clock; write_address; write_enable; write_data }
;;

let zip s t =
  { write_clock = s.write_clock, t.write_clock
  ; write_address = s.write_address, t.write_address
  ; write_enable = s.write_enable, t.write_enable
  ; write_data = s.write_data, t.write_data
  }
;;

let iter2 s t ~f = iter (zip s t) ~f:(fun (s, t) -> f s t)
let map2 s t ~f = map (zip s t) ~f:(fun (s, t) -> f s t)
let to_list t = [ t.write_clock; t.write_address; t.write_enable; t.write_data ]

let port_names =
  { write_clock = "write_clock"
  ; write_address = "write_address"
  ; write_enable = "write_enable"
  ; write_data = "write_data"
  }
;;
