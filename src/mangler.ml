open! Import

type t =
  { case_sensitive : bool
  ; table : (string, int) Hashtbl.t
  }
[@@deriving sexp_of]

let create ~case_sensitive =
  { case_sensitive
  ; table =
      Hashtbl.create
        (if case_sensitive then (module String) else (module String.Caseless))
  }
;;

let add_identifier t name = Hashtbl.add t.table ~key:name ~data:0

let add_identifiers_exn t names =
  List.iter names ~f:(fun name ->
    match add_identifier t name with
    | `Ok -> ()
    | `Duplicate ->
      raise_s
        [%message
          "Failed to add identifier to mangler as it is already present"
            ~invalid_identifier:(name : string)])
;;

let find_index t = Hashtbl.find t.table

let rec mangle t name =
  match find_index t name with
  | None ->
    Hashtbl.add_exn t.table ~key:name ~data:0;
    name
  | Some i ->
    Hashtbl.set t.table ~key:name ~data:(i + 1);
    mangle t (name ^ "_" ^ Int.to_string i)
;;
