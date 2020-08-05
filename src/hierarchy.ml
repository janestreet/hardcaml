open! Import

let hierarchy
      (type i o)
      (module I : Interface.S_Of_signal with type Of_signal.t = i)
      (module O : Interface.S_Of_signal with type Of_signal.t = o)
  =
  let create_inst = Instantiation.create_with_interface (module I) (module O) in
  let create_circuit_exn = Circuit.create_with_interface (module I) (module O) in
  Circuit.with_create_options
    (fun create_options
      ?port_checks
      ?add_phantom_inputs
      ?instance
      db
      ~name
      create_fn
      inputs
      ->
        let circuit =
          Circuit.call_with_create_options
            create_circuit_exn
            create_options
            ?port_checks
            ?add_phantom_inputs
            ~name
            create_fn
        in
        let name = Circuit_database.insert db circuit in
        create_inst ?instance ~name inputs)
;;

let create ~scope ~name create_fn inputs = create_fn (Scope.sub_scope scope name) inputs

let hierarchical
      (type i o)
      (module I : Interface.S_Of_signal with type Of_signal.t = i)
      (module O : Interface.S_Of_signal with type Of_signal.t = o)
  =
  let hierarchy = hierarchy (module I) (module O) in
  Circuit.with_create_options
    (fun create_options
      ?port_checks
      ?add_phantom_inputs
      ?instance
      ~(scope : Scope.t)
      ~name
      create_fn
      inputs
      ->
        let sub_scope_name =
          match instance with
          | None -> name
          | Some name -> name
        in
        if Scope.flatten_design scope
        then create ~scope ~name:sub_scope_name create_fn inputs
        else
          Circuit.call_with_create_options
            hierarchy
            create_options
            ?port_checks
            ?add_phantom_inputs
            ?instance
            (Scope.circuit_database scope)
            ~name
            (create_fn (Scope.sub_scope scope sub_scope_name))
            inputs)
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  let create = hierarchy (module I) (module O)
end

module In_scope (I : Interface.S) (O : Interface.S) = struct
  type create = Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  let create = create
  let hierarchical = hierarchical (module I) (module O)
end
