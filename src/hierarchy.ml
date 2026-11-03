open! Core0

let fold circuit database ~init ~f =
  let rec fold arg (circuit : Circuit.t) inst =
    let arg = f arg (Some circuit) inst in
    List.fold
      (Circuit.instantiations circuit)
      ~init:arg
      ~f:(fun arg { instantiation = inst; _ } ->
        match Circuit_database.find database ~mangled_name:inst.circuit_name with
        | Some circuit -> fold arg circuit (Some inst)
        | None -> f arg None (Some inst))
  in
  fold init circuit None
;;

let print circuit database =
  let rec f level circuit instance_name =
    Stdio.printf "%s%s(%s)\n" level (Circuit.name circuit) instance_name;
    List.iter (Circuit.instantiations circuit) ~f:(fun { instantiation = inst; _ } ->
      let next_level = "  " ^ level in
      match Circuit_database.find database ~mangled_name:inst.circuit_name with
      | Some circuit -> f next_level circuit inst.instance_label
      | None ->
        Stdio.printf
          "%s%s(%s) [no implementation]\n"
          next_level
          inst.circuit_name
          inst.instance_label)
  in
  f "" circuit "top"
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  let validate_circuit_against_interface circuit =
    let circuit_inputs =
      Circuit.inputs circuit
      |> List.map ~f:(fun s -> Signal.names s |> List.hd_exn)
      |> Set.of_list (module String)
    in
    let interface_inputs = Set.of_list (module String) I.Names_and_widths.port_names in
    let input_ports_in_circuit_but_not_interface =
      Set.diff circuit_inputs interface_inputs
    in
    let circuit_name = Circuit.name circuit in
    if not (Set.is_empty input_ports_in_circuit_but_not_interface)
    then
      raise_s
        [%message
          "Error while instantiating module hierarchy"
            (circuit_name : string)
            (input_ports_in_circuit_but_not_interface : Set.M(String).t)]
  ;;

  let create
    ?attributes
    ?input_attributes
    ?output_attributes
    ?config
    ?instance
    db
    ~name
    create_fn
    inputs
    =
    let module Instantiation = Instantiation.With_interface (I) (O) in
    let module Circuit = Circuit.With_interface (I) (O) in
    let create_inst = Instantiation.create in
    let circuit =
      Circuit.create_exn ?config ?input_attributes ?output_attributes ~name create_fn
    in
    validate_circuit_against_interface circuit;
    let name = Circuit_database.insert db circuit in
    create_inst ?instance ?attributes ~name inputs
  ;;
end

module How_to_instantiate = struct
  type t =
    | Inlined
    | Inlined_in_scope
    | Hierarchical
    | Hierarchical_or_inlined_by_scope
  [@@deriving sexp_of]
end

module In_scope_shared (I : Interface.S) (O : Interface.S) = struct
  module Hierarchy = With_interface (I) (O)

  let create_in_scope ~scope ~name create_fn inputs =
    let scope = Scope.sub_scope scope name in
    let label_ports = Scope.auto_label_hierarchical_ports scope in
    let ( -- ) = Scope.naming scope in
    let ( -- ) p s n f =
      let s = Signal.wireof s -- (p ^ Scope.Path.default_path_seperator ^ n) in
      Signal.( --$ ) s f
    in
    let inputs =
      if label_ports
      then I.map3 inputs I.port_names I.wave_formats ~f:(( -- ) "i")
      else inputs
    in
    let outputs = create_fn scope inputs in
    if label_ports
    then O.map3 outputs O.port_names O.wave_formats ~f:(( -- ) "o")
    else outputs
  ;;

  (* Convert a filename to a reasonable module name for use in the hierarchy *)
  let default_module_name filename =
    let name = Stdlib.Filename.basename filename |> Stdlib.Filename.chop_extension in
    let valid_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '$' in
    String.map name ~f:(fun c -> if valid_char c then c else '_')
  ;;

  let derive_here_name here =
    if phys_equal here Lexing.dummy_pos
    then "unnamed_hierarchical_module_add_explicit_name"
    else default_module_name here.pos_fname
  ;;

  let hierarchical
    ?config
    ?instance
    ?attributes
    ?input_attributes
    ?output_attributes
    ?(how_to_instantiate = How_to_instantiate.Hierarchical_or_inlined_by_scope)
    ?name
    ~(here : [%call_pos])
    ~(scope : Scope.t)
    create_fn
    inputs
    =
    let name =
      match name with
      | Some name -> name
      | None -> derive_here_name here
    in
    let instance =
      match instance with
      | None -> name
      | Some name -> name
    in
    let in_scope () = create_in_scope ~scope ~name:instance create_fn inputs in
    let hierarchy () =
      let scope = Scope.hierarchy_sub_scope scope instance in
      let instance = Scope.instance scope in
      (* If the caller did not provide a config, derive one from the sub-scope so that any
         properties created within this hierarchical circuit are associated with it (and
         not the parent circuit). *)
      let config =
        match config with
        | Some _ as config -> config
        | None -> Some (Scope.circuit_config_for_hierarchy scope)
      in
      Hierarchy.create
        ?attributes
        ?input_attributes
        ?output_attributes
        ?config
        ?instance
        (Scope.circuit_database scope)
        ~name
        (create_fn scope)
        inputs
    in
    match how_to_instantiate with
    | Inlined -> create_fn scope inputs
    | Inlined_in_scope -> in_scope ()
    | Hierarchical -> hierarchy ()
    | Hierarchical_or_inlined_by_scope ->
      if Scope.flatten_design scope then in_scope () else hierarchy ()
  ;;
end

module In_scope (I : Interface.S) (O : Interface.S) = struct
  include In_scope_shared (I) (O)

  type create_fn = Scope.t -> Interface.Create_fn(I)(O).t

  let hierarchical = hierarchical
end
