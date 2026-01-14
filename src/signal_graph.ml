[@@@ocaml.flambda_o3]

open! Core0

module Normalized_signal_uid = struct
  include Signal.Type.Uid
end

let uid = Signal.uid
let deps = (module Signal.Type.Deps : Signal.Type.Deps)

type t =
  { outputs : Signal.t list
  ; upto : Hash_set.M(Signal.Type.Uid).t
  }
[@@deriving sexp_of]

let create ?(upto = []) t =
  { outputs = t
  ; upto = Hash_set.of_list (module Signal.Type.Uid) (List.map upto ~f:Signal.uid)
  }
;;

let depth_first_search
  ?(deps = deps)
  ?(f_before = fun a _ -> a)
  ?(f_after = fun a _ -> a)
  t
  ~init
  =
  let module Deps = (val deps) in
  let set = Hash_set.copy t.upto in
  let rec search1 acc signal =
    if Hash_set.mem set (uid signal)
    then acc
    else (
      Hash_set.add set (uid signal);
      let acc = f_before acc signal in
      let acc = search acc signal in
      let acc = f_after acc signal in
      acc)
  and search acc t = Deps.fold t ~init:acc ~f:search1 in
  List.fold t.outputs ~init ~f:search1
;;

let fold ?deps t ~init ~f = depth_first_search ?deps t ~init ~f_before:f
let iter ?deps t ~f = depth_first_search ?deps t ~f_before:(fun _ s -> f s) ~init:()

let filter ?deps t ~f =
  depth_first_search ?deps t ~init:[] ~f_before:(fun arg signal ->
    if f signal then signal :: arg else arg)
;;

let inputs graph =
  Or_error.try_with (fun () ->
    depth_first_search graph ~init:[] ~f_before:(fun acc signal ->
      let open Signal in
      match signal with
      | Wire { driver = Some _; _ } -> acc
      | Wire { driver = None; _ } ->
        (match names signal with
         | [ _ ] -> signal :: acc
         | [] ->
           raise_s
             [%message
               "circuit input signal must have a port name (unassigned wire?)"
                 ~input_signal:(signal : Signal.t)]
         | _ ->
           raise_s
             [%message
               "circuit input signal should only have one port name"
                 ~input_signal:(signal : Signal.t)])
      | _ -> acc))
;;

let validate_outputs (t : t) =
  Or_error.try_with (fun () ->
    List.iter t.outputs ~f:(fun (output_signal : Signal.t) ->
      let open Signal in
      match output_signal with
      | Wire _ ->
        (match Signal.Type.Deps.to_list output_signal with
         | [] | [ Empty ] ->
           raise_s
             [%message "circuit output signal is not driven" (output_signal : Signal.t)]
         | _ ->
           (match names output_signal with
            | [ _ ] -> ()
            | [] ->
              raise_s
                [%message
                  "circuit output signal must have a port name" (output_signal : Signal.t)]
            | _ ->
              raise_s
                [%message
                  "circuit output signal should only have one port name"
                    (output_signal : Signal.t)]))
      | _ ->
        raise_s
          [%message "circuit output signal must be a wire" (output_signal : Signal.t)]))
;;

let outputs (t : t) = t.outputs

(* [rewrite] maintains a table mapping signals in the input graph to the corresponding
   signal in the output graph. It first creates an entry for each wire in the graph. It
   then does a depth-first search following signal dependencies starting from each wire.
   This terminates because all loops go through wires.

   [f] is called for every signal in the graph. It will be provided with rewritten
   incoming edges. [Wire]s are provided before they are attached to their driver.

   [f_upto] is called on all of the upto signals of the signal graph
*)
let rewrite t ~f ~f_upto =
  let open Signal in
  let expecting_a_wire signal =
    raise_s [%message "expecting a wire (internal error)" (signal : Signal.t)]
  in
  let not_expecting_a_wire signal =
    raise_s [%message "not expecting a wire (internal error)" (signal : Signal.t)]
  in
  let new_signal_by_old_uid = Hashtbl.create (module Signal.Type.Uid) in
  let add_mapping ~old_signal ~new_signal =
    Hashtbl.add_exn new_signal_by_old_uid ~key:(uid old_signal) ~data:new_signal
  in
  let new_signal signal =
    match Hashtbl.find new_signal_by_old_uid (uid signal) with
    | None ->
      raise_s
        [%message "[Signal_graph.rewrite] failed to rewrite signal" (signal : Signal.t)]
    | Some s -> s
  in
  let rec rewrite_signal_upto_wires signal ~seen_uids =
    let uid = uid signal in
    match Hashtbl.find new_signal_by_old_uid uid with
    | Some x -> x
    | None ->
      (match Set.mem seen_uids uid with
       | true ->
         raise_s
           [%message
             "Encountered a loop when rewriting signals"
               (seen_uids : Set.M(Signal.Type.Uid).t)
               (uid : Signal.Type.Uid.t)]
       | false ->
         let new_signal =
           match Hash_set.mem t.upto (Signal.uid signal) with
           | true -> f_upto signal
           | false ->
             (match signal with
              | Wire _ -> not_expecting_a_wire signal
              | _ ->
                f
                  (Signal.Type.map_dependant
                     signal
                     ~f:(rewrite_signal_upto_wires ~seen_uids:(Set.add seen_uids uid))))
         in
         add_mapping ~old_signal:signal ~new_signal;
         new_signal)
  in
  let rewrite_signal_upto_wires =
    rewrite_signal_upto_wires ~seen_uids:(Set.empty (module Signal.Type.Uid))
  in
  (* find wires *)
  let old_wires = filter t ~f:Type.is_wire in
  (* create unattached replacement wires *)
  List.iter old_wires ~f:(fun old_wire ->
    add_mapping
      ~old_signal:old_wire
      ~new_signal:
        (match old_wire with
         | Wire { info; _ } -> f (Wire { info; driver = None })
         | _ -> expecting_a_wire old_wire));
  (* rewrite from every wire and output *)
  List.iter (old_wires @ t.outputs) ~f:(function
    | Wire { driver; _ } ->
      Option.iter driver ~f:(fun driver ->
        ignore (rewrite_signal_upto_wires driver : Signal.t))
    | signal -> ignore (rewrite_signal_upto_wires signal : Signal.t));
  (* re-attach wires *)
  List.iter old_wires ~f:(fun old_wire ->
    match old_wire with
    | Wire { driver; _ } ->
      Option.iter driver ~f:(fun driver ->
        let new_driver = new_signal driver in
        let new_wire = new_signal old_wire in
        Signal.(new_wire <-- new_driver))
    | signal -> expecting_a_wire signal);
  let upto =
    t.upto
    |> Hash_set.to_list
    |> List.map ~f:(fun old_uid ->
      match Hashtbl.find new_signal_by_old_uid old_uid with
      | Some new_upto -> uid new_upto
      | None -> old_uid)
    |> Hash_set.of_list (module Signal.Type.Uid)
  in
  let signal_graph = { outputs = List.map t.outputs ~f:new_signal; upto } in
  let new_signal_by_old_uid =
    Hashtbl.to_alist new_signal_by_old_uid |> Map.of_alist_exn (module Signal.Type.Uid)
  in
  signal_graph, new_signal_by_old_uid
;;

let normalized_uids_generator () =
  (* uid generation (note; 1L and up, 0L reserved for empty) *)
  let `New new_id, _ = Signal.Type.Uid.generator () in
  new_id
;;

let normalize_uids t =
  let fresh_id = normalized_uids_generator () in
  let rewrite_uid ~fresh_id signal =
    let open Signal in
    let update_id id = { id with Type.Info.uid = fresh_id () } in
    Signal.Type.map_info signal ~f:update_id
  in
  rewrite t ~f:(rewrite_uid ~fresh_id) ~f_upto:Fn.id |> fst
;;

let compute_normalized_uids t =
  let fresh_id = normalized_uids_generator () in
  let create_fresh_id norm_ids signal = (fresh_id (), signal) :: norm_ids in
  depth_first_search t ~init:[] ~f_before:create_fresh_id
;;

let fan_out_map t =
  depth_first_search
    t
    ~init:(Map.empty (module Signal.Type.Uid))
    ~f_before:(fun map signal ->
      let target = Signal.uid signal in
      (* [signal] is in the fan_out of all of its [deps] *)
      Signal.Type.Deps.fold signal ~init:map ~f:(fun map source ->
        let source = Signal.uid source in
        let fan_out =
          Map.find map source |> Option.value ~default:Signal.Type.Uid_set.empty
        in
        Map.set map ~key:source ~data:(Set.add fan_out target)))
;;

let fan_in_map t =
  depth_first_search
    t
    ~init:(Map.empty (module Signal.Type.Uid))
    ~f_before:(fun map signal ->
      Signal.Type.Deps.rev_map signal ~f:Signal.uid
      |> Set.of_list (module Signal.Type.Uid)
      |> fun data -> Map.set map ~key:(Signal.uid signal) ~data)
;;

let topological_sort ~deps (graph : t) =
  let module Deps = (val deps : Signal.Type.Deps) in
  let nodes, edges =
    fold graph ~init:([], []) ~f:(fun (nodes, edges) to_ ->
      to_ :: nodes, Deps.map to_ ~f:(fun from -> { Topsort.Edge.from; to_ }) @ edges)
  in
  match Topsort.sort ~nodes ~edges with
  | Ok sorted -> Ok sorted
  | Error (`Cycle cycle) -> Error cycle
;;

module Deps_for_simulation_scheduling = Signal.Type.Make_deps (struct
    let fold (t : Signal.t) ~init ~f =
      match t with
      | Mem_read_port { read_address; _ } -> f init read_address
      | Reg _ -> init
      | Multiport_mem _ -> init
      | Empty
      | Const _
      | Op2 _
      | Mux _
      | Cases _
      | Cat _
      | Not _
      | Wire _
      | Select _
      | Inst _ -> Signal.Type.Deps.fold t ~init ~f
    ;;
  end)

module Deps_without_case_matches = Signal.Type.Make_deps (struct
    let fold (t : Signal.t) ~init ~f =
      match t with
      | Cases { select; cases; default; _ } ->
        let arg = f init select in
        let arg =
          List.fold ~init:arg cases ~f:(fun arg (_match_with, value) -> f arg value)
        in
        let arg = f arg default in
        arg
      | Empty
      | Mem_read_port _
      | Reg _
      | Multiport_mem _
      | Const _
      | Op2 _
      | Mux _
      | Cat _
      | Not _
      | Wire _
      | Select _
      | Inst _ -> Signal.Type.Deps.fold t ~init ~f
    ;;
  end)

module Deps_for_loop_checking = Signal.Type.Make_deps (struct
    let fold (t : Signal.t) ~init ~f =
      match t with
      | Mem_read_port { read_address; _ } -> f init read_address
      | Reg _ -> init
      | Multiport_mem _ -> init
      | Inst _ -> init
      | Empty | Const _ | Op2 _ | Mux _ | Cases _ | Cat _ | Not _ | Wire _ | Select _ ->
        Signal.Type.Deps.fold t ~init ~f
    ;;
  end)

let detect_combinational_loops t =
  match topological_sort ~deps:(module Deps_for_loop_checking) t with
  | Ok _ -> Ok ()
  | Error cycle ->
    Or_error.error_s [%message "Combinational loop" ~_:(cycle : Signal.t list)]
;;

let topological_sort_exn ~deps graph =
  match topological_sort ~deps graph with
  | Ok sorted -> sorted
  | Error cycle -> raise_s [%message "Combinational loop" ~_:(cycle : Signal.t list)]
;;

let last_layer_of_nodes ~is_input graph =
  let module Deps = Deps_for_simulation_scheduling in
  (* DFS signals starting from [graph] until a register (or memory) is reached. While
     traversing, mark all signals that are encountered with a bool to indicate whether the
     signal is in a path between a register (or memory) and the output of the graph--the
     last layer.

     Note that the same map that keeps track of the whether the signal is in the last
     layer also doubles as a visited set for the DFS. *)
  let rec visit_signal ((in_layer, _) : bool Map.M(Signal.Type.Uid).t * bool) signal =
    match Map.find in_layer (uid signal) with
    | Some is_in_layer -> in_layer, is_in_layer
    | None ->
      (* These nodes are not scheduled, so need filtering. They are all terminal nodes
         under scheduling deps. Put them in the map as not in the final layer. *)
      if Signal.Type.is_const signal
         || Signal.Type.is_empty signal
         || Signal.Type.is_mem signal
         || is_input signal
      then
        Map.set in_layer ~key:(uid signal) ~data:false, false
        (* Regs are not in the final layer either, but we can't add them to the map as
           [false]. We will have to recurse to them each time instead. *)
      else if Signal.Type.is_reg signal
      then in_layer, true
      else (
        (* recurse deeper *)
        let in_layer, is_in_layer = fold_signal_deps (in_layer, false) signal in
        let is_in_layer = is_in_layer || Signal.Type.is_mem_read_port signal in
        Map.set in_layer ~key:(uid signal) ~data:is_in_layer, is_in_layer)
  (* In final layer if any dependancy is also in the final layer. *)
  and fold_signal_deps layer signal =
    Deps.fold signal ~init:layer ~f:(fun (in_layer, is_in_layer) signal ->
      let in_layer, is_in_layer' = visit_signal (in_layer, is_in_layer) signal in
      in_layer, is_in_layer || is_in_layer')
  in
  let in_layer, _ =
    List.fold
      ~init:(Map.empty (module Signal.Type.Uid), false)
      graph.outputs
      ~f:visit_signal
  in
  (* Drop nodes not in the final layer. That will track back to an input or constant but
     not be affected by a register or memory. *)
  Map.to_alist in_layer
  |> List.filter_map ~f:(fun (uid, is_in_layer) -> if is_in_layer then Some uid else None)
;;

let resolve_clock_domains t =
  let rec transitively_resolve (signal : Signal.t) =
    match signal with
    | Wire { info = _; driver } ->
      (match driver with
       | None -> signal
       | Some otherwise -> transitively_resolve otherwise)
    | Empty
    | Op2 _
    | Not _
    | Cat _
    | Mux _
    | Cases _
    | Const _
    | Select _
    | Reg _
    | Multiport_mem _
    | Mem_read_port _
    | Inst _ -> raise_s [%message "Invalid clock driver" (signal : Signal.t)]
  in
  let resolve_clock clock_domains signal =
    let uid = uid signal in
    if Map.mem clock_domains uid
    then clock_domains
    else (
      let clock_domain = transitively_resolve signal in
      Map.add_exn clock_domains ~key:uid ~data:clock_domain)
  in
  depth_first_search
    t
    ~init:(Map.empty (module Signal.Type.Uid))
    ~f_before:(fun acc signal ->
      match signal with
      | Reg reg -> resolve_clock acc reg.register.clock.clock
      | Multiport_mem { write_ports; _ } ->
        Array.fold write_ports ~init:acc ~f:(fun acc port ->
          resolve_clock acc port.write_clock)
      | _ -> acc)
;;
