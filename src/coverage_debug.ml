open! Core0

let debug = false
let debug_oc = ref None
let oc_exn () = Option.value_exn !debug_oc

let maybe_print_debug sexp =
  if debug then Out_channel.output_line (oc_exn ()) (force sexp |> Sexp.to_string_hum)
;;

let () =
  if debug
  then (
    debug_oc := Some (Out_channel.create "coverage_debug.txt");
    Stdlib.at_exit (fun () -> Out_channel.close (oc_exn ())))
;;
