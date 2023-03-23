open Base

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
  let width = List.fold outputs ~init:0 ~f:(fun a (_, i) -> a + i) in
  let deps = List.map inputs ~f:snd in
  let outputs, _ =
    List.fold outputs ~init:([], 0) ~f:(fun (o, a) (n, w) -> (n, (w, a)) :: o, a + w)
  in
  let one_output = List.length outputs = 1 in
  let signal =
    Signal.Inst
      { signal_id = Signal.make_id width deps
      ; extra_uid = Signal.new_id ()
      ; instantiation =
          { inst_name = name
          ; inst_instance =
              (match instance with
               | None -> "the_" ^ name
               | Some i -> i)
          ; inst_generics = parameters
          ; inst_inputs = inputs
          ; inst_outputs = outputs
          ; inst_lib = lib
          ; inst_arch = arch
          }
      }
  in
  List.iter attributes ~f:(fun attribute ->
    ignore (Signal.add_attribute signal attribute : Signal.t));
  List.map outputs ~f:(fun (name, (width, offset)) ->
    ( name
      , (* We need to create a distinct output signal - if there is only one output then
           the instantiation and the output signal share a uid which confuses the logic
           for associating attrributes correctly. *)
      if one_output
      then Signal.wireof signal
      else Signal.select signal (offset + width - 1) offset ))
  |> Map.of_alist_exn (module String)
;;

module With_interface (I : Interface.S_Of_signal) (O : Interface.S_Of_signal) = struct
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
    O.Unsafe_assoc_by_port_name.of_alist (Map.to_alist t)
  ;;
end

let create_with_interface
      (type i o)
      (module I : Interface.S_Of_signal with type Of_signal.t = i)
      (module O : Interface.S_Of_signal with type Of_signal.t = o)
  =
  let module I = With_interface (I) (O) in
  I.create
;;
