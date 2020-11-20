open Base

type t =
  { ltl : (string, Property.LTL.path) Hashtbl.t
  ; mutable is_finalized : bool
  ; aps : (Signal.Uid.t, Signal.t) Hashtbl.t
  }
[@@deriving sexp_of]

let create () =
  { ltl = Hashtbl.create (module String)
  ; is_finalized = false
  ; aps = Hashtbl.create (module Signal.Uid)
  }
;;

let add_ltl t name property =
  if t.is_finalized
  then
    raise_s [%message "The property manager is finalized, no new properties can be added"];
  Hashtbl.set t.ltl ~key:name ~data:property;
  Property.LTL.atomic_propositions property
  |> List.iter ~f:(fun signal -> Hashtbl.set t.aps ~key:(Signal.uid signal) ~data:signal)
;;

let finalize t =
  t.is_finalized <- true;
  Map.of_alist_exn (module String) (Hashtbl.to_alist t.ltl)
;;

let atomic_propositions t =
  t.is_finalized <- true;
  Hashtbl.data t.aps
;;
