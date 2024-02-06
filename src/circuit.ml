(* A circuit is defined by transitively following the dependencies of its outputs,
   stopping at unassigned wires or constants.  [Signal_graph.inputs] does this.
   All such unassigned wires are circuit inputs.  As a consequence, all other wires
   in a circuit are assigned, and hence cannot be changed. *)

open Base
module Uid_set = Signal.Type.Uid_set

module Signal_map = struct
  type t = Signal.t Map.M(Signal.Uid).t [@@deriving sexp_of]

  let create graph =
    let add_signal map signal =
      let uid = Signal.uid signal in
      Map.add_exn map ~key:uid ~data:signal
    in
    Signal_graph.depth_first_search
      graph
      ~init:(Map.empty (module Signal.Uid))
      ~f_before:add_signal
  ;;
end

module Instantiation_sexp = struct
  type t = Signal.Type.instantiation

  let sexp_of_t (t : t) = [%message "" (t.inst_name : string) (t.inst_instance : string)]
end

type t =
  { name : string
  ; signal_by_uid : Signal_map.t
  ; inputs : Signal.t list
  ; outputs : Signal.t list
  ; phantom_inputs : (string * int) list
  ; signal_graph : Signal_graph.t
      (* [fan_in] and [fan_out] are lazily computed.  One might worry that this would interact
     poorly with signals, which have some mutable components (e.g. wires).  But those have
     already been set by the time a circuit is created, so a circuit is not mutable. *)
  ; fan_out : Signal.Type.Uid_set.t Map.M(Signal.Uid).t Lazy.t
  ; fan_in : Signal.Type.Uid_set.t Map.M(Signal.Uid).t Lazy.t
  ; assertions : Signal.t Map.M(String).t
  ; instantiations : Instantiation_sexp.t list
  }
[@@deriving fields ~getters, sexp_of]

module Summary = struct
  let sexp_of_signal signal = Signal.Type.sexp_of_signal_recursive ~depth:0 signal

  let sexp_of_t t =
    [%message
      ""
        ~name:(t.name : string)
        ~input_ports:(t.inputs : signal list)
        ~output_ports:(t.outputs : signal list)]
  ;;
end

module Port_checks = struct
  type t =
    | Relaxed
    | Port_sets
    | Port_sets_and_widths
end

module Config = struct
  type t =
    { detect_combinational_loops : bool
    ; normalize_uids : bool
    ; assertions : Assertion_manager.t option
    ; port_checks : Port_checks.t
    ; add_phantom_inputs : bool
    ; modify_outputs : Signal.t list -> Signal.t list
    }

  let default =
    { detect_combinational_loops = true
    ; normalize_uids = true
    ; assertions = None
    ; port_checks = Port_sets_and_widths
    ; add_phantom_inputs = true
    ; modify_outputs = Fn.id
    }
  ;;

  let default_for_simulations =
    { detect_combinational_loops = false
    ; normalize_uids = false
    ; assertions = None
    ; port_checks = Port_sets_and_widths
    ; add_phantom_inputs = true
    ; modify_outputs = Fn.id
    }
  ;;
end

let ok_exn = Or_error.ok_exn

let check_ports_in_one_direction circuit_name direction ports =
  let find_repeated_names ports =
    let rec find seen repeated = function
      | [] -> seen, repeated
      | name :: ports ->
        if Set.mem seen name
        then find seen (Set.add repeated name) ports
        else find (Set.add seen name) repeated ports
    in
    let empty = Set.empty (module String) in
    find empty empty ports
  in
  let port_names =
    List.map ports ~f:(fun s ->
      match Signal.names s with
      | [ name ] ->
        if String.is_empty name
        then raise_s [%message "Port name is an empty string"]
        else name
      | _ ->
        raise_s
          [%message
            (direction ^ "s must have a single name")
              (circuit_name : string)
              (s : Signal.t)])
  in
  let set, repeated = find_repeated_names port_names in
  if not (Set.is_empty repeated)
  then
    raise_s
      [%message
        (direction ^ " port names are not unique")
          (circuit_name : string)
          (repeated : Set.M(String).t)];
  set
;;

let check_port_names_are_well_formed circuit_name inputs outputs =
  let inputs = check_ports_in_one_direction circuit_name "Input" inputs in
  let outputs = check_ports_in_one_direction circuit_name "Output" outputs in
  let input_and_output_names = Set.inter inputs outputs in
  if not (Set.is_empty input_and_output_names)
  then
    raise_s
      [%message
        "Port names are not unique"
          (circuit_name : string)
          (input_and_output_names : Set.M(String).t)]
;;

let create_exn ?(config = Config.default) ~name outputs =
  Instantiation.Expert.validate_module_name name;
  let assertions =
    match config.assertions with
    | Some assertions -> Assertion_manager.finalize assertions
    | None -> Map.empty (module String)
  in
  let output_names =
    List.concat_map outputs ~f:(fun signal ->
      if Signal.is_empty signal then [] else Signal.names signal)
    |> Set.of_list (module String)
  in
  (* We have to filter out the assertions that are already in the output because they are
     from [Cyclesim_with_properties.With_interface.create]

     We add the assertions as output to make sure they show up on the waveform and their signals
     are not optimized away *)
  let output_assertions =
    assertions
    |> Map.to_alist
    |> List.filter ~f:(fun (n, _) -> not (Set.mem output_names n))
    |> List.map ~f:(fun (n, s) -> Signal.output n s)
  in
  let outputs = outputs @ output_assertions in
  let signal_graph = Signal_graph.create outputs in
  (* check that all outputs are assigned wires with 1 name *)
  ignore (ok_exn (Signal_graph.outputs ~validate:true signal_graph) : Signal.t list);
  (* uid normalization *)
  let signal_graph =
    if config.normalize_uids
    then Signal_graph.normalize_uids signal_graph
    else signal_graph
  in
  (* get new output wires *)
  let outputs = Signal_graph.outputs signal_graph |> ok_exn in
  (* get inputs checking that they are valid *)
  let inputs = ok_exn (Signal_graph.inputs signal_graph) in
  (* update the assertions map to the new normalized signals and find all instantiations. *)
  let assertions, instantiations =
    Signal_graph.fold
      signal_graph
      ~init:(assertions, [])
      ~f:(fun (assertions, instantiations) signal ->
      if Signal.is_empty signal
      then assertions, instantiations
      else (
        let assertions =
          List.fold (Signal.names signal) ~init:assertions ~f:(fun assertions name ->
            Map.change assertions name ~f:(function
              | Some _ -> Some signal
              | None -> None))
        in
        let instantiations =
          match signal with
          | Signal.Type.Inst inst -> inst.instantiation :: instantiations
          | _ -> instantiations
        in
        assertions, instantiations))
  in
  (* check for combinational loops *)
  if config.detect_combinational_loops
  then ok_exn (Signal_graph.detect_combinational_loops signal_graph);
  (* Ensure input and output port names are unique *)
  check_port_names_are_well_formed name inputs outputs;
  (* construct the circuit *)
  { name
  ; signal_by_uid = Signal_map.create signal_graph
  ; inputs
  ; outputs
  ; phantom_inputs = []
  ; signal_graph
  ; fan_out = lazy (Signal_graph.fan_out_map signal_graph)
  ; fan_in = lazy (Signal_graph.fan_in_map signal_graph)
  ; assertions
  ; instantiations
  }
;;

let set_phantom_inputs circuit phantom_inputs =
  (* Remove phantom inputs that are already inputs, and disallow phantom inputs
     that have the same name as an output. *)
  let module Port = struct
    module T = struct
      type t = string * int [@@deriving sexp_of]

      let compare (n0, _) (n1, _) = String.compare n0 n1
    end

    include T
    include Comparable.Make (T)

    let of_signal port =
      let name =
        match Signal.names port with
        | [ name ] -> name
        | _ ->
          raise_s
            [%message
              "Ports should have one name" (port : Signal.t) (circuit : Summary.t)]
      in
      name, Signal.width port
    ;;
  end
  in
  let inputs = List.map circuit.inputs ~f:Port.of_signal |> Set.of_list (module Port) in
  let outputs = List.map circuit.outputs ~f:Port.of_signal |> Set.of_list (module Port) in
  let phantom = Set.of_list (module Port) phantom_inputs in
  let phantom_inputs = Set.diff phantom inputs in
  if not (Set.is_empty (Set.inter phantom_inputs outputs))
  then
    raise_s
      [%message
        "Phantom input is also a circuit output"
          (phantom_inputs : Set.M(Port).t)
          (outputs : Set.M(Port).t)
          (circuit : Summary.t)];
  { circuit with phantom_inputs = phantom_inputs |> Set.to_list }
;;

let with_name t ~name = { t with name }
let uid_equal a b = Signal.Uid.equal (Signal.uid a) (Signal.uid b)
let is_input t signal = List.mem t.inputs signal ~equal:uid_equal
let is_output t signal = List.mem t.outputs signal ~equal:uid_equal
let find_signal_exn t uid = Map.find_exn t.signal_by_uid uid
let fan_out_map t = Lazy.force t.fan_out
let fan_in_map t = Lazy.force t.fan_in
let signal_map c = c.signal_by_uid
let assertions t = t.assertions

let structural_compare ?check_names c0 c1 =
  (* Number of inputs and outputs match *)
  let num_ports_match which_ports =
    List.length (which_ports c0) = List.length (which_ports c1)
  in
  (* Name and width (and implicitly, order) of ports match *)
  let ports_match which_ports =
    let names_or_empty = function
      | Signal.Type.Empty -> []
      | s -> Signal.names s
    in
    match
      List.fold2 (which_ports c0) (which_ports c1) ~init:true ~f:(fun b p0 p1 ->
        b
        && [%compare.equal: string list] (names_or_empty p0) (names_or_empty p1)
        && Signal.width p0 = Signal.width p1)
    with
    | Ok ok -> ok
    | Unequal_lengths -> false
  in
  let recurse_into_circuit () =
    snd
      (List.fold2_exn
         (outputs c0)
         (outputs c1)
         ~init:(Uid_set.empty, true)
         ~f:(fun (set, b) s t ->
         let set, b' =
           Signal.Type.structural_compare ?check_names ~initial_deps:set s t
         in
         set, b && b'))
  in
  num_ports_match inputs
  && num_ports_match outputs
  (* outputs, including names, are the same *)
  && ports_match outputs
  && ports_match inputs
  && (* check full structural comparision from each output *)
  recurse_into_circuit ()
;;

let instantiations t = t.instantiations

module With_interface (I : Interface.S_Of_signal) (O : Interface.S_Of_signal) = struct
  type create = I.Of_signal.t -> O.Of_signal.t

  let check_io_port_sets_match circuit =
    let actual_ports ports =
      List.map ports ~f:Signal.names |> List.concat |> Set.of_list (module String)
    in
    let actual_input_ports = inputs circuit |> actual_ports in
    let actual_input_ports =
      phantom_inputs circuit
      |> List.map ~f:fst
      |> Set.of_list (module String)
      |> Set.union actual_input_ports
    in
    let actual_output_ports = outputs circuit |> actual_ports in
    let expected_input_ports =
      I.Names_and_widths.port_names |> Set.of_list (module String)
    in
    let expected_output_ports =
      O.Names_and_widths.port_names |> Set.of_list (module String)
    in
    let check direction actual_ports expected_ports =
      let expected_but_not_in_circuit = Set.diff expected_ports actual_ports in
      let in_circuit_but_not_expected = Set.diff actual_ports expected_ports in
      if (not (Set.is_empty expected_but_not_in_circuit))
         || not (Set.is_empty in_circuit_but_not_expected)
      then
        raise_s
          [%message
            "Port sets do not match"
              (direction : string)
              (expected_ports : Set.M(String).t)
              (actual_ports : Set.M(String).t)
              (expected_but_not_in_circuit : Set.M(String).t)
              (in_circuit_but_not_expected : Set.M(String).t)
              (circuit : Summary.t)]
    in
    check "input" actual_input_ports expected_input_ports;
    check "output" actual_output_ports expected_output_ports
  ;;

  let check_widths_match circuit =
    let port (intf : (string * int) list) s =
      match Signal.names s with
      | [ name ] ->
        (match List.Assoc.find intf name ~equal:String.equal with
         | None ->
           raise_s
             [%message
               "Signal was a circuit port, but was not listed in the interface"
                 (name : string)
                 (intf : (string * int) list)]
         | Some expected_width ->
           let port_width = Signal.width s in
           if port_width <> expected_width
           then
             raise_s
               [%message
                 "Port width of "
                   (name : string)
                   (port_width : int)
                   " was specified as "
                   (expected_width : int)
                   " in interface"])
      | _ ->
        raise_s
          [%message
            "[Circuit.With_interface.check_widths_match] Unexpected error - invalid port \
             name(s)"
              (circuit : Summary.t)]
    in
    inputs circuit |> List.iter ~f:(port I.Names_and_widths.port_names_and_widths);
    outputs circuit |> List.iter ~f:(port O.Names_and_widths.port_names_and_widths)
  ;;

  let check_io_port_sets_and_widths_match circuit =
    check_io_port_sets_match circuit;
    check_widths_match circuit
  ;;

  let move_port_attributes from_ to_ =
    Signal.Type.set_attributes to_ (Signal.Type.get_attributes from_);
    Signal.Type.set_attributes from_ []
  ;;

  let check_alist_of_one_direction name direction alist =
    ignore
      (check_ports_in_one_direction name direction (List.map ~f:snd alist)
        : Set.M(String).t)
  ;;

  let create_exn ?(config = Config.default) ~name logic =
    let circuit_inputs =
      let ports =
        List.map I.Names_and_widths.port_names_and_widths ~f:(fun (n, b) ->
          n, Signal.input n b)
      in
      check_alist_of_one_direction name "Input" ports;
      I.Unsafe_assoc_by_port_name.of_alist ports
    in
    let inputs = I.map circuit_inputs ~f:Signal.wireof in
    let outputs = logic inputs in
    let circuit_outputs =
      let ports =
        List.map2_exn O.Names_and_widths.port_names (O.to_list outputs) ~f:(fun n s ->
          n, Signal.output n s)
      in
      check_alist_of_one_direction name "Output" ports;
      O.Unsafe_assoc_by_port_name.of_alist ports
    in
    I.iter2 inputs circuit_inputs ~f:move_port_attributes;
    O.iter2 outputs circuit_outputs ~f:move_port_attributes;
    let circuit =
      create_exn ~config ~name (config.modify_outputs (O.to_list circuit_outputs))
    in
    let circuit =
      if config.add_phantom_inputs
      then set_phantom_inputs circuit I.Names_and_widths.port_names_and_widths
      else circuit
    in
    (match config.port_checks with
     | Relaxed -> ()
     | Port_sets -> check_io_port_sets_match circuit
     | Port_sets_and_widths -> check_io_port_sets_and_widths_match circuit);
    circuit
  ;;
end

let create_with_interface
  (type i o)
  (module I : Interface.S_Of_signal with type Of_signal.t = i)
  (module O : Interface.S_Of_signal with type Of_signal.t = o)
  =
  let module C = With_interface (I) (O) in
  C.create_exn
;;
