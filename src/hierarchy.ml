open! Import

module With_interface (I : Interface.S) (O : Interface.S) = struct
  module Inst = Instantiation.With_interface (I) (O)
  module Circ = Circuit.With_interface (I) (O)

  let create =
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
              Circ.create_exn
              create_options
              ?port_checks
              ?add_phantom_inputs
              ~name
              create_fn
          in
          let name = Circuit_database.insert db circuit in
          Inst.create ?instance ~name inputs)
  ;;
end

module In_scope (I : Interface.S) (O : Interface.S) = struct
  type create = Scope.t -> Signal.t Interface.Create_fn(I)(O).t

  module Hierarchy = With_interface (I) (O)

  let create ~scope ~name create_fn inputs =
    create_fn (Scope.sub_scope scope name) inputs
  ;;

  let hierarchical =
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
              Hierarchy.create
              create_options
              ?port_checks
              ?add_phantom_inputs
              ?instance
              (Scope.circuit_database scope)
              ~name
              (create_fn (Scope.sub_scope scope sub_scope_name))
              inputs)
  ;;
end
