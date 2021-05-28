open Base

type t =
  { mutable map : Signal.t Map.M(String).t
  ; mutable is_finalized : bool
  }
[@@deriving sexp_of]

let create () = { map = Map.empty (module String); is_finalized = false }

let add t name asn =
  match t.is_finalized with
  | false -> t.map <- Map.add_exn t.map ~key:name ~data:asn
  | true ->
    raise_s [%message "Assertion manager is finalized, adding assertions is not allowed"]
;;

let finalize t =
  t.is_finalized <- true;
  t.map
;;

let of_signals signals = { map = signals; is_finalized = true }
