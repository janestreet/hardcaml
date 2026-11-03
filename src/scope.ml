open! Core0

module Path = struct
  type t = string list [@@deriving sexp_of]

  let default_path_seperator = "$"
  let create () = []
  let push path name = name :: path
  let to_string ?(sep = default_path_seperator) path = String.concat ~sep (List.rev path)
  let to_list path = path
  let concat path1 path2 = path2 @ path1

  let concat_str ~sep path s =
    [ List.rev path
    ; (match s with
       | "" -> []
       | _ -> [ s ])
    ]
    |> List.concat
    |> String.concat ~sep
  ;;
end

module Trace_properties = Property_manager.Trace_properties

type t =
  { instance_path : Path.t (* Track the current instantiation path *)
  ; name_path : Path.t (* Track local naming paths within an instance *)
  ; circuit_database : (Circuit_database.t[@sexp.opaque])
  ; flatten_design : bool
  ; trace_properties : Trace_properties.t
  ; auto_label_hierarchical_ports : bool
  ; instantiation_mangler : Mangler.t
  ; property_manager : Property_manager.t
  }
[@@deriving fields ~getters, sexp_of]

(* Choose whether mangling of instantiation names is case sensitive or not. *)
let case_sensitive = false

let create
  ?(flatten_design = false)
  ?(auto_label_hierarchical_ports = false)
  ?(trace_properties = Trace_properties.none)
  ()
  =
  { instance_path = Path.create ()
  ; name_path = Path.create ()
  ; circuit_database = Circuit_database.create ()
  ; flatten_design
  ; trace_properties
  ; auto_label_hierarchical_ports
  ; instantiation_mangler = Mangler.create ~case_sensitive
  ; property_manager = Property_manager.create ()
  }
;;

module Push_path = struct
  type t =
    | Instance
    | Name
end

let sub_scope_impl
  ~reinitialize_instantiation_mangler
  ~reinitialize_property_manager
  ~(push_path : Push_path.t)
  scope
  name
  =
  let name = Mangler.mangle scope.instantiation_mangler name in
  { scope with
    instance_path =
      (match push_path with
       | Instance -> Path.push scope.instance_path name
       | Name -> scope.instance_path)
  ; name_path =
      (* Local name paths are reset on hierarchical instantiations *)
      (match push_path with
       | Instance -> Path.create ()
       | Name -> Path.push scope.name_path name)
  ; instantiation_mangler =
      (if reinitialize_instantiation_mangler
       then Mangler.create ~case_sensitive
       else scope.instantiation_mangler)
  ; property_manager =
      (if reinitialize_property_manager
       then Property_manager.create ()
       else scope.property_manager)
  }
;;

let sub_scope =
  sub_scope_impl
    ~reinitialize_instantiation_mangler:true
    ~reinitialize_property_manager:false
    ~push_path:Name
;;

let hierarchy_sub_scope =
  sub_scope_impl
    ~reinitialize_instantiation_mangler:true
    ~reinitialize_property_manager:true
    ~push_path:Instance
;;

let path scope =
  if scope.flatten_design
  then Path.concat scope.instance_path scope.name_path
  else scope.name_path
;;

let name ?(sep = Path.default_path_seperator) scope n =
  Path.concat_str ~sep (path scope) n
;;

let instance (scope : t) = List.hd scope.instance_path
let naming ?sep scope ~(loc : [%call_pos]) s n = Signal.( -- ) ~loc s (name ?sep scope n)

let make_ltl_ap scope name signal =
  let wire = Signal.wireof signal in
  let wire = naming scope wire ("ap" ^ Path.default_path_seperator ^ name) in
  Property.LTL.Formula.p wire
;;

(* Properties are only recorded when the relevant property type is enabled in the scope's
   [trace_properties]. The property specification itself is still built unconditionally by
   the caller; we only avoid storing it (which both matches the "tracing off does nothing"
   behaviour and avoids spurious name clashes in designs that don't trace properties). *)

let add_assertion ?group scope assertion_name assertion =
  if scope.trace_properties.enable_assertions
  then (
    let assertion_name = name scope assertion_name in
    Property_manager.add_assertion ?group scope.property_manager assertion_name assertion)
;;

let add_ltl_property ?group scope property_name property =
  if scope.trace_properties.enable_ltl
  then (
    let property_name = name scope property_name in
    Property_manager.add_ltl ?group scope.property_manager property_name property)
;;

let add_sva_property ?group scope property_name property =
  if scope.trace_properties.enable_sva
  then (
    let property_name = name scope property_name in
    Property_manager.add_sva ?group scope.property_manager property_name property)
;;

let property_manager scope =
  (* Return the live manager (not a filtered copy) so that callers that capture it before
     the design is elaborated still observe the properties that get added. Consumers that
     care about the trace settings (e.g. RTL generation) filter via [Trace_properties.t]
     held in the circuit config. *)
  if Trace_properties.any_enabled scope.trace_properties
  then Some scope.property_manager
  else None
;;

let circuit_config_with_properties ?circuit_config scope =
  let trace_properties = scope.trace_properties in
  let for_sim = Trace_properties.for_simulation trace_properties in
  let circuit_config =
    (* Choose an appropriate circuit config *)
    match circuit_config, for_sim with
    | Some circuit_config, _ -> circuit_config
    | None, true -> Circuit.Config.default_for_simulations
    | None, false -> Circuit.Config.default
  in
  if not (Trace_properties.any_enabled trace_properties)
  then
    (* If no properties are enabled in the scope, don't add the property manager. *)
    circuit_config
  else
    { circuit_config with
      (* Pass the (live) property manager and the trace settings - [Circuit] filters the
         properties by type and group when the circuit is created. *)
      property_manager = Some scope.property_manager
    ; trace_properties
    ; port_checks = (if for_sim then Relaxed else circuit_config.port_checks)
    }
;;

let circuit_config_for_hierarchy scope = circuit_config_with_properties scope
