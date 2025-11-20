open! Core0

let fold circuit database ~init ~f =
  let rec fold arg (circuit : Circuit.t) inst =
    let arg = f arg (Some circuit) inst in
    List.fold (Circuit.instantiations circuit) ~init:arg ~f:(fun arg inst ->
      match Circuit_database.find database ~mangled_name:inst.circuit_name with
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

module Caller_signal_type = struct
  type 'signal t =
    | Clocked : Clocked_signal.t t
    | Signal : Signal.t t
end

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

  let names_and_locs s =
    try Signal.names_and_locs s with
    | _ -> []
  ;;

  (* Traverse the contents of a sub-circuit and rewrite names with the local path. The
     full paths build up as we move up the hierarchy. *)
  let auto_labelling ~instance ~inputs ~outputs =
    Signal_graph.iter
      (Signal_graph.create ~upto:(I.to_list inputs) (O.to_list outputs))
      ~f:(fun s ->
        Signal.set_names
          s
          (List.map (names_and_locs s) ~f:(fun n ->
             { n with name = instance ^ "$" ^ n.name })));
    outputs
  ;;

  let auto_naming scope = Scope.Naming_scheme.equal (Scope.naming_scheme scope) Auto

  let create_in_scope ~scope ~name create_fn inputs =
    let scope = Scope.sub_scope scope name in
    let label_ports = Scope.auto_label_hierarchical_ports scope in
    let ( -- ) = if auto_naming scope then Signal.( -- ) else Scope.naming scope in
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
    let in_scope () =
      if auto_naming scope
      then
        auto_labelling
          ~instance
          ~inputs
          ~outputs:(create_in_scope ~scope ~name:instance create_fn inputs)
      else create_in_scope ~scope ~name:instance create_fn inputs
    in
    let hierarchy () =
      let scope = Scope.sub_scope scope instance in
      let instance = Scope.instance scope in
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

  module Clock_domain_interface_utils (X : Interface.S) = struct
    let build_mapping_from_spec_to_exact
      ~ports_description
      ~on_all_constants
      ~(specification : _ X.t)
      (actual : _ X.t)
      =
      X.to_list (X.zip specification actual)
      |> Clock_domain.construct_mapping_from_spec_to_exact
      |> Map.map ~f:(fun data ->
        let%tydi { specification_domain; maps_to } = data in
        match maps_to with
        | Ok (Exact runtime) -> runtime
        | Ok All_constants -> on_all_constants specification_domain
        | Error (Maps_to_multiple _) ->
          let port_names =
            X.map3 X.port_names specification actual ~f:(fun name port_spec actual ->
              if Clock_domain.equal port_spec specification_domain
              then Some (name, actual)
              else None)
            |> X.to_list
            |> List.filter_opt
          in
          let message =
            Printf.sprintf
              "The following %s ports are expected to have the same clock domain, but do \
               not"
              ports_description
          in
          raise_s
            [%message
              message
                ~clock_domain:(specification_domain : Clock_domain.t)
                ~_:(port_names : (string * Clock_domain.Runtime.t) list)]
        | Error Maps_to_unknown ->
          let port_names_with_unknown_clock_domains =
            X.map2 X.port_names actual ~f:(fun name actual ->
              match actual with
              | Unknown -> Some name
              | Constant | Exact _ -> None)
            |> X.to_list
            |> List.filter_opt
          in
          let message =
            Printf.sprintf
              "The following %s ports are expected to have the same clock domain, but \
               one of them is unexpectedly an unknown."
              ports_description
          in
          raise_s
            [%message
              message
                ~clock_domain:(specification_domain : Clock_domain.t)
                (port_names_with_unknown_clock_domains : string list)])
    ;;
  end

  module Clock_I = Clock_domain_interface_utils (I)
  module Clock_O = Clock_domain_interface_utils (O)

  (* This handles the case where the user passes in an input_domains_specification and the
     input signals are clocked. There are 3 types of valid cases:

     - All the signals that have the same Clock_domain.t specification have the same exact
       runtime domain
     - All the signals that have the same Clock_domain.t specification have a mix of a
       common runtime domain and the constant runtime domain
     - All the signals that have the same Clock_domain.t specification have a constant
       runtime domains

     The first two cases are simple -- we just map to spec to the one exact runtime
     domain. For the third case, we choose to regenerate a new fresh domain, rather than
     propogate the Constant domain. This is definitely sound, possibly overly strict. But
     it shouldn't happen often enough that we have chosen to opt for a conservative
     solution for now.
  *)
  let validate_input_clock_domain_specification_against_runtime_domains
    ~(specification : Clock_domain.t I.t)
    (actual : Clock_domain.Runtime.t I.t)
    =
    let mapping =
      Clock_I.build_mapping_from_spec_to_exact
        ~ports_description:"input"
        ~on_all_constants:Clock_domain.generate_fresh_exact_domain
        ~specification
        actual
    in
    let remapped_runtime_clock_domains =
      I.map specification ~f:(fun spec ->
        Clock_domain.Runtime.Exact (Map.find_exn mapping (Clock_domain.uid spec)))
    in
    remapped_runtime_clock_domains, mapping
  ;;

  (* This function validates two things:

     - The output ports' clock domains are consistent amongst themselves wrt the
       specification (independent of the input clock domain)
     - The output ports' with a particular clock domain spec has the same runtime clock
       domain as input ports with the same clock domain spec
  *)
  let validate_outputs_clock_domain_consistent_with_mapping_and_specification
    ~(mapping_inferred_from_input_domain_specification :
        Clock_domain.Exact.t Clock_domain.Uid.Map.t)
    ~(specification : Clock_domain.t O.t)
    (actual : Clock_domain.Runtime.t O.t)
    : Clock_domain.Runtime.t O.t
    =
    let mapping_inferred_from_output_domain_specification =
      let on_all_constants spec =
        match
          Map.find
            mapping_inferred_from_input_domain_specification
            (Clock_domain.uid spec)
        with
        | None -> Clock_domain.generate_fresh_exact_domain spec
        | Some exact -> exact
      in
      Clock_O.build_mapping_from_spec_to_exact
        ~ports_description:"output"
        ~on_all_constants
        ~specification
        actual
    in
    O.map3
      O.port_names
      specification
      actual
      ~f:(fun port_name specification actual_runtime ->
        let spec_uid = Clock_domain.uid specification in
        let expected_exact =
          match Map.find mapping_inferred_from_input_domain_specification spec_uid with
          | None ->
            (* Entering this branch means we encountered a clock domain specification that
               wasn't associated to any ports in the input. In this case, we use the clock
               domain implied from the output gruoping.
               [mapping_inferred_from_output_domain_specification] is a total map from all
               the specifications that shows up in the output domains specification, so
               this find_exn should always succeed.
            *)
            Map.find_exn mapping_inferred_from_output_domain_specification spec_uid
          | Some expected_exact ->
            (* In the event an infered input runtime domain exists, checking that the
               output value's runtime clock domain being equal to that is sufficient to
               validate the two criterion above.
            *)
            expected_exact
        in
        (match actual_runtime with
         | Constant -> ()
         | Unknown ->
           let message =
             Printf.sprintf
               !"The output port %s has an Unknown clock domain. Unknown clock domains \
                 are not allowed to cross interface boundaries"
               port_name
           in
           raise_s
             [%message
               message ~expected_clock_domain:(expected_exact : Clock_domain.Exact.t)]
         | Exact actual_exact ->
           if not (Clock_domain.Exact.equal expected_exact actual_exact)
           then (
             let message =
               Printf.sprintf
                 "The output port %s has an incorrect clock domain."
                 port_name
             in
             raise_s
               [%message
                 message
                   ~provided:(actual_exact : Clock_domain.Exact.t)
                   ~expected:(expected_exact : Clock_domain.Exact.t)]));
        Clock_domain.Runtime.Exact expected_exact)
  ;;

  let infer_output_clock_domains
    ~(output_domains : Clock_domain.t O.t)
    ~(input_clock_domain_spec_to_runtime_mapping :
        Clock_domain.Exact.t Map.M(Clock_domain.Uid).t)
    =
    let output_clock_domain_spec_to_runtime_mapping = ref [] in
    O.map output_domains ~f:(fun specification_domain ->
      match
        Map.find
          input_clock_domain_spec_to_runtime_mapping
          (Clock_domain.uid specification_domain)
      with
      | Some domain -> Clock_domain.Runtime.Exact domain
      | None ->
        (match
           List.Assoc.find
             !output_clock_domain_spec_to_runtime_mapping
             specification_domain
             ~equal:(fun a b ->
               Clock_domain.Uid.equal (Clock_domain.uid a) (Clock_domain.uid b))
         with
         | Some runtime_domain -> runtime_domain
         | None ->
           let runtime_domain =
             Clock_domain.Runtime.Exact
               (Clock_domain.generate_fresh_exact_domain specification_domain)
           in
           output_clock_domain_spec_to_runtime_mapping
           := (specification_domain, runtime_domain)
              :: !output_clock_domain_spec_to_runtime_mapping;
           runtime_domain))
  ;;
end

module In_clocked_scope
    (I : Interface.S_with_clock_domains)
    (O : Interface.S_with_clock_domains) =
struct
  include In_scope_shared (I) (O)

  type create_fn = Scope.t -> Clocked_signal.t I.t -> Clocked_signal.t O.t

  let hierarchical
    (type caller_signal_type)
    ?config
    ?instance
    ?attributes
    ?input_attributes
    ?output_attributes
    ?how_to_instantiate
    ?name
    ~(caller_signal_type : caller_signal_type Caller_signal_type.t)
    ~(here : [%call_pos])
    ~scope
    (create_fn : create_fn)
    (inputs : caller_signal_type I.t)
    : caller_signal_type O.t
    =
    let input_clock_domains, clock_domain_spec_to_runtime_mapping =
      match caller_signal_type with
      | Clocked ->
        let runtime_clock_domains_on_inputs = I.map inputs ~f:Clocked_signal.get_domain in
        (* Perform validation of runtime domains against the specification. *)
        let runtime, mapping =
          validate_input_clock_domain_specification_against_runtime_domains
            ~specification:I.domains
            runtime_clock_domains_on_inputs
        in
        runtime, mapping
      | Signal ->
        let spec_to_runtime_mapping =
          I.to_list I.domains
          |> Clock_domain.dedup_by_uid
          |> List.map ~f:(fun clock_domain_spec ->
            ( Clock_domain.uid clock_domain_spec
            , Clock_domain.generate_fresh_exact_domain clock_domain_spec ))
          |> Map.of_alist_exn (module Clock_domain.Uid)
        in
        let clock_domains =
          I.map I.domains ~f:(fun spec ->
            Clock_domain.Runtime.Exact
              (Map.find_exn spec_to_runtime_mapping (Clock_domain.uid spec)))
        in
        clock_domains, spec_to_runtime_mapping
    in
    (* Construct the (unclocked) instantiation from the clocked [create_fn]. *)
    let created_output_domains = ref None in
    let unclocked_outputs =
      hierarchical
        ?config
        ?instance
        ?attributes
        ?input_attributes
        ?output_attributes
        ?how_to_instantiate
        ?name
        ~here
        ~scope
        (fun scope unclocked_inputs ->
          (* Lift the unclocked inputs to be clocked by the given input domains. *)
          let clocked_inputs =
            I.map2 input_clock_domains unclocked_inputs ~f:(fun dom unclocked ->
              Clocked_signal.to_clocked unclocked ~dom)
          in
          let clocked_outputs = create_fn scope clocked_inputs in
          (* Capture the output clock domains. *)
          created_output_domains
          := Some (O.map clocked_outputs ~f:Clocked_signal.get_domain);
          O.map clocked_outputs ~f:(fun x -> fst (Clocked_signal.to_rep x)))
        (match caller_signal_type with
         | Signal -> inputs
         | Clocked -> I.map inputs ~f:(fun x -> fst (Clocked_signal.to_rep x)))
    in
    (* Get the output domains. Forcing this is fine - it must have been set. *)
    let created_output_domains = Option.value_exn !created_output_domains in
    (* Validate output domains against the domain specification *)
    let output_domains =
      validate_outputs_clock_domain_consistent_with_mapping_and_specification
        ~mapping_inferred_from_input_domain_specification:
          clock_domain_spec_to_runtime_mapping
        ~specification:O.domains
        created_output_domains
    in
    match caller_signal_type with
    | Signal -> unclocked_outputs
    | Clocked ->
      O.map2 unclocked_outputs output_domains ~f:(fun unclocked dom ->
        Clocked_signal.to_clocked unclocked ~dom)
  ;;
end

module In_scope (I : Interface.S) (O : Interface.S) = struct
  include In_scope_shared (I) (O)

  type create_fn = Scope.t -> Interface.Create_fn(I)(O).t

  let hierarchical = hierarchical

  let hierarchical_unclocked
    (type caller_signal_type)
    ?config
    ?instance
    ?attributes
    ?input_attributes
    ?output_attributes
    ?how_to_instantiate
    ?name
    ?input_domains
    ?output_domains
    ~(caller_signal_type : caller_signal_type Caller_signal_type.t)
    ~(here : [%call_pos])
    ~scope
    (create_fn : create_fn)
    (inputs : caller_signal_type I.t)
    : caller_signal_type O.t
    =
    match caller_signal_type with
    | Signal ->
      hierarchical
        ?config
        ?instance
        ?attributes
        ?input_attributes
        ?output_attributes
        ?how_to_instantiate
        ?name
        ~here
        ~scope
        create_fn
        inputs
    | Clocked ->
      let input_domains, output_domains =
        match input_domains, output_domains with
        | Some input_domains, Some output_domains -> input_domains, output_domains
        | Some _, None | None, Some _ | None, None ->
          raise_s
            [%message
              "When calling [hierarchical_unclocked] with [caller_signal_type = \
               Clocked], you must provide clock domain specifications for both inputs \
               and outputs"]
      in
      let _, input_clock_domain_spec_to_runtime_mapping =
        validate_input_clock_domain_specification_against_runtime_domains
          ~specification:input_domains
          (I.map inputs ~f:Clocked_signal.get_domain)
      in
      let outputs =
        hierarchical
          ?config
          ?instance
          ?attributes
          ?input_attributes
          ?output_attributes
          ?how_to_instantiate
          ?name
          ~here
          ~scope
          create_fn
          (I.map inputs ~f:(fun s ->
             Clocked_signal.unwrap_signal s ~dom:(Clocked_signal.get_domain s)))
      in
      infer_output_clock_domains
        ~output_domains
        ~input_clock_domain_spec_to_runtime_mapping
      |> O.map2 outputs ~f:(fun signal dom -> Clocked_signal.to_clocked signal ~dom)
  ;;

  let single_clock_domain = Clock_domain.create "single_clocked_domain"

  let hierarchical_single_clock_domain
    (type caller_signal_type)
    ?config
    ?instance
    ?attributes
    ?input_attributes
    ?output_attributes
    ?how_to_instantiate
    ?name
    ~(caller_signal_type : caller_signal_type Caller_signal_type.t)
    ~(here : [%call_pos])
    ~scope
    (create_fn : create_fn)
    (inputs : caller_signal_type I.t)
    : caller_signal_type O.t
    =
    hierarchical_unclocked
      ?config
      ?instance
      ?attributes
      ?input_attributes
      ?output_attributes
      ?how_to_instantiate
      ?name
      ~input_domains:(I.const single_clock_domain)
      ~output_domains:(O.const single_clock_domain)
      ~caller_signal_type
      ~here
      ~scope
      create_fn
      inputs
  ;;
end
