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
    name, Signal.select signal (offset + width - 1) offset)
  |> Map.of_alist_exn (module String)
;;

module With_interface (I : Interface.S_Of_signal) (O : Interface.S_Of_signal) = struct
  let create ?lib ?arch ?instance ?parameters ?attributes ~name inputs =
    let inputs =
      List.map2_exn I.Names_and_widths.t (I.to_list inputs) ~f:(fun (n, _) s -> n, s)
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
        ~outputs:O.Names_and_widths.t
    in
    List.map O.Names_and_widths.port_names ~f:(fun name -> name, Map.find_exn t name)
    |> O.of_alist
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
