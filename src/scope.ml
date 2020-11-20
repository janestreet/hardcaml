open Base

module Path = struct
  type t = string list [@@deriving sexp_of]

  let default_path_seperator = "$"
  let create () = []
  let push path name = name :: path
  let to_string ?(sep = default_path_seperator) path = String.concat ~sep (List.rev path)
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
  ; trace_properties : bool
  ; naming_scheme : Naming_scheme.t
  ; auto_label_hierarchical_ports : bool
  ; assertion_manager : Assertion_manager.t
  ; instantiation_mangler : Mangler.t
  ; property_manager : Property_manager.t
  }
[@@deriving fields, sexp_of]

(* Choose whether mangling of instantiation names is case sensitive or not. *)
let case_sensitive = false

let create
      ?(flatten_design = false)
      ?(auto_label_hierarchical_ports = false)
      ?(trace_properties = false)
      ?naming_scheme
      ?name
      ()
  =
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
  ; trace_properties
  ; naming_scheme
  ; auto_label_hierarchical_ports
  ; assertion_manager = Assertion_manager.create ()
  ; instantiation_mangler = Mangler.create ~case_sensitive
  ; property_manager = Property_manager.create ()
  }
;;

let sub_scope scope name =
  let name = Mangler.mangle scope.instantiation_mangler name in
  { scope with
    path = Path.push scope.path name
  ; name_path =
      (match scope.naming_scheme with
       | Full_path -> Path.push scope.path name
       | Local_path -> Path.push (Path.create ()) name
       | No_path -> Path.create ())
  ; instantiation_mangler = Mangler.create ~case_sensitive
  }
;;

let name ?(sep = Path.default_path_seperator) scope n =
  let path = name_path scope in
  match path with
  | [] -> n
  | _ ->
    let path = Path.to_string ~sep path in
    path ^ sep ^ n
;;

let instance (scope : t) = List.hd scope.path
let naming ?sep scope s n = Signal.( -- ) s (name ?sep scope n)

let add_assertion scope asn_name asn =
  if not scope.trace_properties
  then ()
  else (
    let asn_name = name scope asn_name in
    Assertion_manager.add scope.assertion_manager asn_name asn)
;;

let make_ltl_ap scope name signal =
  let wire = Signal.wireof signal in
  let wire = naming scope wire ("ap" ^ Path.default_path_seperator ^ name) in
  Property.LTL.p wire
;;

let add_ltl_property scope property_name property =
  if not scope.trace_properties
  then ()
  else (
    let property_name = name scope property_name in
    Property_manager.add_ltl scope.property_manager property_name property)
;;

let assertion_manager scope = scope.assertion_manager
let property_manager scope = scope.property_manager

let assert_signal_in_always scope asn_name asn =
  let asn_var = Always.Variable.wire ~default:(Signal.one 1) in
  add_assertion scope asn_name (Always.Variable.value asn_var);
  Always.( <-- ) asn_var asn
;;

