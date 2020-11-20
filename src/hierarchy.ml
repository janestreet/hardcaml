open Base

let hierarchy
      (type i o)
      (module I : Interface.S_Of_signal with type Of_signal.t = i)
      (module O : Interface.S_Of_signal with type Of_signal.t = o)
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
  let name = Circuit_database.insert db circuit in
  create_inst ?instance ~name inputs
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
      ~(scope : Scope.t)
      ~name
      create_fn
      inputs
  =
  let hierarchy = hierarchy (module I) (module O) in
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

module With_interface (I : Interface.S) (O : Interface.S) = struct
  let create = hierarchy (module I) (module O)
end

module In_scope (I : Interface.S) (O : Interface.S) = struct
  type create = Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  let create ~scope ~name create_fn inputs =
    let scope = Scope.sub_scope scope name in
    let label_ports = Scope.auto_label_hierarchical_ports scope in
    let ( -- ) = Scope.naming scope in
    let ( -- ) p s n = Signal.wireof s -- (p ^ Scope.Path.default_path_seperator ^ n) in
    let inputs =
      if label_ports then I.map2 inputs I.port_names ~f:(( -- ) "i") else inputs
    in
    let outputs = create_fn scope inputs in
    if label_ports then O.map2 outputs O.port_names ~f:(( -- ) "o") else outputs
  ;;

  let hierarchical ?config ?instance ~(scope : Scope.t) ~name create_fn inputs =
    let hierarchy = hierarchy (module I) (module O) in
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
end
