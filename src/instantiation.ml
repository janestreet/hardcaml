open! Core0

type t =
  { signal : Signal.t
  ; outputs : Signal.t Map.M(String).t
  }

type output_map = Signal.t Map.M(String).t

let module_name_special_chars = String.to_list "_$"
let instance_name_special_chars = String.to_list "_$.[]"
let instantiation_signal t = t.signal
let outputs t = t.outputs
let output t name = Map.find_exn t.outputs name

let validate_module_or_instantiation_name ~special_chars name =
  let is_special c = List.mem special_chars c ~equal:Char.equal in
  let alpha_or_special c = Char.is_alpha c || is_special c in
  let alphanum_or_special c = Char.is_alphanum c || is_special c in
  if String.length name = 0
  then raise_s [%message "Module or instance names cannot be empty"];
  if not (alpha_or_special name.[0])
  then
    raise_s
      [%message
        "First letter of module or instance names should be alpha or special"
          (name : string)
          (special_chars : char list)];
  if not (String.fold name ~init:true ~f:(fun ok c -> ok && alphanum_or_special c))
  then
    raise_s
      [%message
        "Invalid module or instance name - should only contain alphanumeric or special \
         characters"
          (name : string)
          (special_chars : char list)]
;;

let create
  ?(lib = "work")
  ?(arch = "rtl")
  ?instance
  ?(parameters = [])
  ?(attributes = [])
  ()
  ~name
  ~inputs
  ~outputs
  =
  (* filter empty/0 width IOs *)
  (* {[
       let inputs = List.filter (fun (_, s) -> s <> Empty) inputs in
       let outputs = List.filter (fun (_, b) -> b <> 0) outputs in
     ]} *)
  validate_module_or_instantiation_name ~special_chars:module_name_special_chars name;
  Option.iter
    instance
    ~f:(validate_module_or_instantiation_name ~special_chars:instance_name_special_chars);
  let width = List.fold outputs ~init:0 ~f:(fun a (_, i) -> a + i) in
  let outputs, _ =
    List.fold outputs ~init:([], 0) ~f:(fun (o, a) (n, w) ->
      ( ({ name = n; output_width = w; output_low_index = a } : Signal.Type.Inst.Output.t)
        :: o
      , a + w ))
  in
  let outputs = List.rev outputs in
  let one_output = List.length outputs = 1 in
  let signal =
    (* When there are no output ports, this is a zero width signal.

       I wonder if there might be some confusion with [Signal.Empty]? We have tried to
       discourage this for a while in the api (make_id will raise). *)
    Signal.Type.Inst
      { info = Signal.Type.make_id_allow_zero_width width
      ; instantiation =
          { circuit_name = name
          ; instance_label =
              (match instance with
               | None -> "the_" ^ name
               | Some i -> i)
          ; parameters
          ; inputs =
              List.map inputs ~f:(fun (name, input_signal) : _ Signal.Type.Inst.Input.t ->
                { name; input_signal })
          ; outputs
          ; vhdl_instance = { library_name = lib; architecture_name = arch }
          }
      }
  in
  List.iter attributes ~f:(fun attribute ->
    ignore (Signal.add_attribute signal attribute : Signal.t));
  let outputs =
    List.map outputs ~f:(fun { name; output_width = width; output_low_index = offset } ->
      ( name
      , (* We need to create a distinct output signal - if there is only one output then
           the instantiation and the output signal share a uid which confuses the logic
           for associating attrributes correctly. *)
        if one_output
        then Signal.wireof signal
        else Signal.select signal ~high:(offset + width - 1) ~low:offset ))
    |> Map.of_alist_exn (module String)
  in
  { signal; outputs }
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  let create ?lib ?arch ?instance ?parameters ?attributes ~name inputs =
    (* ensure the passed inputs are of the correct widths. *)
    I.Of_signal.validate inputs;
    let inputs =
      List.map2_exn I.Names_and_widths.port_names (I.to_list inputs) ~f:(fun n s -> n, s)
    in
    let t =
      create
        ()
        ?lib
        ?arch
        ?instance
        ?parameters
        ?attributes
        ~name
        ~inputs
        ~outputs:O.Names_and_widths.port_names_and_widths
    in
    O.Unsafe_assoc_by_port_name.of_alist (Map.to_alist (outputs t))
  ;;
end

module Expert = struct
  let validate_module_name n =
    validate_module_or_instantiation_name ~special_chars:module_name_special_chars n
  ;;
end
