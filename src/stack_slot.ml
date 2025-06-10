open Base
module Printexc = Stdlib.Printexc

type location = Printexc.location
type t = Printexc.Slot.t

let format_location (l : location) =
  Printf.sprintf "%s:%i:%i" l.filename l.line_number l.start_char
;;

let format_loc t =
  Option.value_map (Printexc.Slot.location t) ~f:format_location ~default:""
;;

let format t =
  let name = Option.value (Printexc.Slot.name t) ~default:"" in
  Printf.sprintf "%s$%s" name (format_loc t)
;;

let sexp_of_t t = [%sexp (format_loc t : string)]

let compare t1 t2 =
  let flatten t =
    Option.map (Printexc.Slot.location t) ~f:(fun loc ->
      loc.filename, loc.line_number, loc.start_char)
  in
  [%compare: (string * int * int) option] (flatten t1) (flatten t2)
;;

let equal t1 t2 = compare t1 t2 = 0
let hash_fold_t state t = String.hash_fold_t state (format t)
let hash t = String.hash (format t)
