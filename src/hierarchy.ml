open Base

let validate_circuit_against_interface
  (type i)
  (module I : Interface.S_Of_signal with type Of_signal.t = i)
  circuit
  =
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

let hierarchy
  (type i o)
  (module I : Interface.S_Of_signal with type Of_signal.t = i)
  (module O : Interface.S_Of_signal with type Of_signal.t = o)
  ?attributes
  ?config
  ?instance
  db
  ~name
  create_fn
  inputs
  =
  let create_inst = Instantiation.create_with_interface (module I) (module O) in
  let create_circuit_exn = Circuit.create_with_interface (module I) (module O) in
  let circuit = create_circuit_exn ?config ~name create_fn in
  validate_circuit_against_interface (module I) circuit;
  let name = Circuit_database.insert db circuit in
  create_inst ?instance ?attributes ~name inputs
;;

let create ~scope ~name create_fn inputs =
  let scope = Scope.sub_scope scope name in
  create_fn scope inputs
;;

let hierarchical
  (type i o)
  (module I : Interface.S_Of_signal with type Of_signal.t = i)
  (module O : Interface.S_Of_signal with type Of_signal.t = o)
  ?config
  ?instance
  ?attributes
  ~(scope : Scope.t)
  ~name
  create_fn
  inputs
  =
  let hierarchy = hierarchy ?attributes (module I) (module O) in
  let instance =
    match instance with
    | None -> name
    | Some name -> name
  in
  if Scope.flatten_design scope
  then create ~scope ~name:instance create_fn inputs
  else (
    let scope = Scope.sub_scope scope instance in
    let instance = Scope.instance scope in
    hierarchy
      ?config
      ?instance
      (Scope.circuit_database scope)
      ~name
      (create_fn scope)
      inputs)
;;

let fold circuit database ~init ~f =
  let rec fold arg (circuit : Circuit.t) inst =
    let arg = f arg (Some circuit) inst in
    List.fold (Circuit.instantiations circuit) ~init:arg ~f:(fun arg inst ->
      match Circuit_database.find database ~mangled_name:inst.inst_name with
      | Some circuit -> fold arg circuit (Some inst)
      | None -> f arg None (Some inst))
  in
  fold init circuit None
;;

let print circuit database =
  let rec f level circuit instance_name =
    Stdio.printf "%s%s(%s)\n" level (Circuit.name circuit) instance_name;
    List.iter (Circuit.instantiations circuit) ~f:(fun inst ->
      let next_level = "  " ^ level in
      match Circuit_database.find database ~mangled_name:inst.inst_name with
      | Some circuit -> f next_level circuit inst.inst_instance
      | None ->
        Stdio.printf
          "%s%s(%s) [no implementation]\n"
          next_level
          inst.inst_name
          inst.inst_instance)
  in
  f "" circuit "top"
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  let create = hierarchy (module I) (module O)
end

module In_scope (I : Interface.S) (O : Interface.S) = struct
  type create = Scope.t -> Interface.Create_fn(I)(O).t

  let names s =
    try Signal.names s with
    | _ -> []
  ;;

  (* Traverse the contents of a sub-circuit and rewrite names with the local path. The
     full paths build up as we move up the hierarchy. *)
  let auto_labelling ~instance ~inputs ~outputs =
    Signal_graph.iter
      (Signal_graph.create ~upto:(I.to_list inputs) (O.to_list outputs))
      ~f:(fun s ->
        Signal.set_names s (List.map (names s) ~f:(fun n -> instance ^ "$" ^ n)));
    outputs
  ;;

  let auto_naming scope = Scope.Naming_scheme.equal (Scope.naming_scheme scope) Auto

  let create ~scope ~name create_fn inputs =
    let scope = Scope.sub_scope scope name in
    let label_ports = Scope.auto_label_hierarchical_ports scope in
    let ( -- ) = if auto_naming scope then Signal.( -- ) else Scope.naming scope in
    let ( -- ) p s n = Signal.wireof s -- (p ^ Scope.Path.default_path_seperator ^ n) in
    let inputs =
      if label_ports then I.map2 inputs I.port_names ~f:(( -- ) "i") else inputs
    in
    let outputs = create_fn scope inputs in
    if label_ports then O.map2 outputs O.port_names ~f:(( -- ) "o") else outputs
  ;;

  let hierarchical ?config ?instance ?attributes ~(scope : Scope.t) ~name create_fn inputs
    =
    let hierarchy = hierarchy ?attributes (module I) (module O) in
    let instance =
      match instance with
      | None -> name
      | Some name -> name
    in
    if Scope.flatten_design scope
    then
      if auto_naming scope
      then
        auto_labelling
          ~instance
          ~inputs
          ~outputs:(create ~scope ~name:instance create_fn inputs)
      else create ~scope ~name:instance create_fn inputs
    else (
      let scope = Scope.sub_scope scope instance in
      let instance = Scope.instance scope in
      hierarchy
        ?config
        ?instance
        (Scope.circuit_database scope)
        ~name
        (create_fn scope)
        inputs)
  ;;
end
