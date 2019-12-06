open! Import

type instobj = < i : string -> Signal.t ; o : string -> Signal.t >

let create
      ?(lib = "work")
      ?(arch = "rtl")
      ?instance
      ?(parameters = [])
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
  let find name =
    let w, o = List.Assoc.find_exn outputs name ~equal:String.equal in
    Signal.select signal (o + w - 1) o
  in
  object
    method i name = List.Assoc.find_exn inputs name ~equal:String.equal

    method o name = find name
  end
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  let create ?lib ?arch ?instance ?parameters ~name inputs =
    let t =
      create
        ()
        ?lib
        ?arch
        ?instance
        ?parameters
        ~name
        ~inputs:(I.to_list (I.map2 I.t inputs ~f:(fun (n, _) s -> n, s)))
        ~outputs:(O.to_list O.t)
    in
    O.map O.t ~f:(fun (n, _) -> t#o n)
  ;;
end
