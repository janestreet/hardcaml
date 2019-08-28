open! Import

module Path = struct
  type t = string list [@@deriving sexp_of]

  let create () = []
  let push path name = name :: path
  let to_string ?(sep = "$") path = String.concat ~sep (List.rev path)
  let to_list path = path
end

module Naming_scheme = struct
  type t =
    | Full_path
    | Local_path
    | No_path
  [@@deriving sexp_of]
end

type t =
  { path : Path.t
  ; name_path : Path.t
  ; circuit_database : (Circuit_database.t[@sexp.opaque])
  ; flatten_design : bool
  ; naming_scheme : Naming_scheme.t
  }
[@@deriving fields, sexp_of]

let create ?(flatten_design = false) ?naming_scheme ?name () =
  let naming_scheme =
    match (naming_scheme : Naming_scheme.t option) with
    | Some naming_scheme -> naming_scheme
    | None -> if flatten_design then Full_path else No_path
  in
  let path = Path.create () in
  let path =
    match name with
    | None -> path
    | Some name -> Path.push path name
  in
  { path
  ; name_path = path
  ; circuit_database = Circuit_database.create ()
  ; flatten_design
  ; naming_scheme
  }
;;

let sub_scope scope name =
  { scope with
    path = Path.push scope.path name
  ; name_path =
      (match scope.naming_scheme with
       | Full_path -> Path.push scope.path name
       | Local_path -> Path.push (Path.create ()) name
       | No_path -> Path.create ())
  }
;;

let name ?(sep = "$") scope n =
  let path = name_path scope in
  match path with
  | [] -> n
  | _ ->
    let path = Path.to_string path in
    path ^ sep ^ n
;;

let naming ?sep scope s n = Signal.( -- ) s (name ?sep scope n)
