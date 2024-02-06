[@@@ocaml.flambda_o3]

open Base

let uid = Signal.uid
let deps = (module Signal.Type.Deps : Signal.Type.Deps)

type t =
  { outputs : Signal.t list
  ; upto : Hash_set.M(Signal.Uid).t
  }
[@@deriving sexp_of]

let create ?(upto = []) t =
  { outputs = t
  ; upto = Hash_set.of_list (module Signal.Uid) (List.map upto ~f:Signal.uid)
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

let fold t ~init ~f = depth_first_search t ~init ~f_before:f
let iter t ~f = depth_first_search t ~f_before:(fun _ s -> f s) ~init:()

let filter t ~f =
  depth_first_search t ~init:[] ~f_before:(fun arg signal ->
    if f signal then signal :: arg else arg)
;;

let inputs graph =
  Or_error.try_with (fun () ->
    depth_first_search graph ~init:[] ~f_before:(fun acc signal ->
      let open Signal in
      match signal with
      | Wire { driver; _ } ->
        if not (Signal.is_empty !driver)
        then acc
        else (
          match names signal with
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

let outputs ?(validate = false) (t : t) =
  Or_error.try_with (fun () ->
    if validate
    then
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
                    "circuit output signal must have a port name"
                      (output_signal : Signal.t)]
              | _ ->
                raise_s
                  [%message
                    "circuit output signal should only have one port name"
                      (output_signal : Signal.t)]))
        | _ ->
          raise_s
            [%message "circuit output signal must be a wire" (output_signal : Signal.t)]);
    t.outputs)
;;

(* [normalize_uids] maintains a table mapping signals in the input graph to
   the corresponding signal in the output graph.  It first creates an entry for
   each wire in the graph.  It then does a depth-first search following signal
   dependencies starting from each wire.  This terminates because all loops
   go through wires. *)
let normalize_uids t =
  let open Signal in
  let expecting_a_wire signal =
    raise_s [%message "expecting a wire (internal error)" (signal : Signal.t)]
  in
  let not_expecting_a_wire signal =
    raise_s [%message "not expecting a wire (internal error)" (signal : Signal.t)]
  in
  let new_signal_by_old_uid = Hashtbl.create (module Uid) in
  let add_mapping ~old_signal ~new_signal =
    Hashtbl.add_exn new_signal_by_old_uid ~key:(uid old_signal) ~data:new_signal
  in
  let new_signal signal =
    match Hashtbl.find new_signal_by_old_uid (uid signal) with
    | None ->
      raise_s
        [%message
          "[Signal_graph.normalize_uids] failed to rewrite signal" (signal : Signal.t)]
    | Some s -> s
  in
  (* uid generation (note; 1L and up, 0L reserved for empty) *)
  let fresh_id =
    let `New new_id, _ = Signal.Uid.generator () in
    new_id
  in
  let rec rewrite_signal_upto_wires signal =
    match Hashtbl.find new_signal_by_old_uid (uid signal) with
    | Some x -> x
    | None ->
      let update_id id = { id with Type.s_id = fresh_id () } in
      let new_signal =
        match signal with
        | Empty -> Type.Empty
        | Const { signal_id; constant } ->
          Const { signal_id = update_id signal_id; constant }
        | Op2 { signal_id; op; arg_a; arg_b } ->
          let arg_a = rewrite_signal_upto_wires arg_a in
          let arg_b = rewrite_signal_upto_wires arg_b in
          Op2 { signal_id = update_id signal_id; op; arg_a; arg_b }
        | Mux { signal_id; select; cases } ->
          let select = rewrite_signal_upto_wires select in
          let cases = List.map cases ~f:rewrite_signal_upto_wires in
          Mux { signal_id = update_id signal_id; select; cases }
        | Cat { signal_id; args } ->
          let args = List.map args ~f:rewrite_signal_upto_wires in
          Cat { signal_id = update_id signal_id; args }
        | Not { signal_id; arg } ->
          let arg = rewrite_signal_upto_wires arg in
          Not { signal_id = update_id signal_id; arg }
        | Select { signal_id; arg; high; low } ->
          let arg = rewrite_signal_upto_wires arg in
          Select { signal_id = update_id signal_id; arg; high; low }
        | Reg { signal_id; register; d } ->
          let d = rewrite_signal_upto_wires d in
          let register =
            let reg_clock = rewrite_signal_upto_wires register.reg_clock in
            let reg_clock_edge = register.reg_clock_edge in
            let reg_reset = rewrite_signal_upto_wires register.reg_reset in
            let reg_reset_edge = register.reg_reset_edge in
            let reg_reset_value = rewrite_signal_upto_wires register.reg_reset_value in
            let reg_clear = rewrite_signal_upto_wires register.reg_clear in
            let reg_clear_level = register.reg_clear_level in
            let reg_clear_value = rewrite_signal_upto_wires register.reg_clear_value in
            let reg_enable = rewrite_signal_upto_wires register.reg_enable in
            { Type.reg_clock
            ; reg_clock_edge
            ; reg_reset
            ; reg_reset_edge
            ; reg_reset_value
            ; reg_clear
            ; reg_clear_level
            ; reg_clear_value
            ; reg_enable
            }
          in
          Reg { signal_id = update_id signal_id; register; d }
        | Multiport_mem { signal_id; size; write_ports } ->
          let rewrite_write_port (write_port : _ Write_port.t) =
            let write_clock = rewrite_signal_upto_wires write_port.write_clock in
            let write_address = rewrite_signal_upto_wires write_port.write_address in
            let write_data = rewrite_signal_upto_wires write_port.write_data in
            let write_enable = rewrite_signal_upto_wires write_port.write_enable in
            { Write_port.write_clock; write_address; write_enable; write_data }
          in
          let write_ports = Array.map write_ports ~f:rewrite_write_port in
          Multiport_mem { signal_id = update_id signal_id; size; write_ports }
        | Mem_read_port { signal_id; memory; read_address } ->
          let read_address = rewrite_signal_upto_wires read_address in
          let memory = rewrite_signal_upto_wires memory in
          Mem_read_port { signal_id = update_id signal_id; memory; read_address }
        | Inst { signal_id; instantiation; _ } ->
          let inputs =
            List.map instantiation.inst_inputs ~f:(fun (name, input) ->
              name, rewrite_signal_upto_wires input)
          in
          Inst
            { signal_id = update_id signal_id
            ; extra_uid = fresh_id ()
            ; instantiation = { instantiation with inst_inputs = inputs }
            }
        | Wire _ -> not_expecting_a_wire signal
      in
      add_mapping ~old_signal:signal ~new_signal;
      new_signal
  in
  (* find wires *)
  let old_wires = filter t ~f:Type.is_wire in
  (* create unattached replacement wires *)
  List.iter old_wires ~f:(fun old_wire ->
    add_mapping
      ~old_signal:old_wire
      ~new_signal:
        (match old_wire with
         | Wire { signal_id; _ } ->
           Wire
             { signal_id = { signal_id with s_id = fresh_id () }
             ; driver = ref Signal.empty
             }
         | _ -> expecting_a_wire old_wire));
  (* rewrite from every wire *)
  List.iter old_wires ~f:(function
    | Wire { driver; _ } -> ignore (rewrite_signal_upto_wires !driver : Signal.t)
    | signal -> expecting_a_wire signal);
  (* re-attach wires *)
  List.iter old_wires ~f:(fun old_wire ->
    match old_wire with
    | Wire { driver; _ } ->
      if not (Signal.is_empty !driver)
      then (
        let new_driver = new_signal !driver in
        let new_wire = new_signal old_wire in
        Signal.(new_wire <== new_driver))
    | signal -> expecting_a_wire signal);
  { t with outputs = List.map t.outputs ~f:new_signal }
;;

let fan_out_map t =
  depth_first_search
    t
    ~init:(Map.empty (module Signal.Uid))
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
    ~init:(Map.empty (module Signal.Uid))
    ~f_before:(fun map signal ->
      Signal.Type.Deps.rev_map signal ~f:Signal.uid
      |> Set.of_list (module Signal.Uid)
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
    | Empty | Const _ | Op2 _ | Mux _ | Cat _ | Not _ | Wire _ | Select _ | Inst _ ->
      Signal.Type.Deps.fold t ~init ~f
  ;;
end)

module Deps_for_loop_checking = Signal.Type.Make_deps (struct
  let fold (t : Signal.t) ~init ~f =
    match t with
    | Mem_read_port { read_address; _ } -> f init read_address
    | Reg _ -> init
    | Multiport_mem _ -> init
    | Inst _ -> init
    | Empty | Const _ | Op2 _ | Mux _ | Cat _ | Not _ | Wire _ | Select _ ->
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
  (* DFS signals starting from [graph] until a register (or memory) is reached.
     While traversing, mark all signals that are encountered with a bool to
     indicate whether the signal is in a path between a register (or memory)
     and the output of the graph--the last layer.

     Note that the same map that keeps track of the whether the signal is in the last
     layer also doubles as a visited set for the DFS. *)
  let rec visit_signal ((in_layer, _) : bool Map.M(Signal.Uid).t * bool) signal =
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
           [false].  We will have to recurse to them each time instead. *)
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
    List.fold ~init:(Map.empty (module Signal.Uid), false) graph.outputs ~f:visit_signal
  in
  (* Drop nodes not in the final layer. That will track back to an input or constant but
     not be affected by a register or memory. *)
  Map.to_alist in_layer
  |> List.filter_map ~f:(fun (uid, is_in_layer) -> if is_in_layer then Some uid else None)
;;
