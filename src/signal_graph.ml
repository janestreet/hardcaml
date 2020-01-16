open! Import

let uid = Signal.uid
let deps = Signal.deps

type t = Signal.t list [@@deriving sexp_of]

let create t = t

let depth_first_search
      ?(deps = deps)
      ?(f_before = fun a _ -> a)
      ?(f_after = fun a _ -> a)
      t
      ~init
  =
  let rec search1 signal ~set acc =
    if Set.mem set (uid signal)
    then acc, set
    else (
      let set = Set.add set (uid signal) in
      let acc = f_before acc signal in
      let acc, set = search (deps signal) acc ~set in
      let acc = f_after acc signal in
      acc, set)
  and search t acc ~set =
    List.fold t ~init:(acc, set) ~f:(fun (arg, set) s -> search1 s ~set arg)
  in
  fst (search t init ~set:(Set.empty (module Signal.Uid)))
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

let outputs ?(validate = false) t =
  Or_error.try_with (fun () ->
    if validate
    then
      List.iter t ~f:(fun (output_signal : Signal.t) ->
        let open Signal in
        match output_signal with
        | Wire _ ->
          (match deps output_signal with
           | [] | [ Empty ] ->
             raise_s
               [%message
                 "circuit output signal is not driven" (output_signal : Signal.t)]
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
            [%message
              "circuit output signal must be a wire" (output_signal : Signal.t)]);
    t)
;;

let detect_combinational_loops t =
  Or_error.try_with (fun () ->
    let open Signal in
    let module Signal_state = struct
      type t =
        | Unvisited
        | Visiting
        | Visited
      [@@deriving sexp_of]
    end
    in
    let state_by_uid : (Uid.t, Signal_state.t ref) Hashtbl.t =
      Hashtbl.create (module Uid)
    in
    let signal_state signal =
      Hashtbl.find_or_add state_by_uid (uid signal) ~default:(fun _ ->
        ref Signal_state.Unvisited)
    in
    let set_state signal state = signal_state signal := state in
    (* Registers, memories etc are always in the [Visited] state. *)
    let breaks_a_cycle s = is_reg s || is_mem s || is_inst s || is_empty s in
    let initial_visited = filter t ~f:breaks_a_cycle in
    List.iter initial_visited ~f:(fun signal -> set_state signal Visited);
    let rec dfs signal =
      let state = signal_state signal in
      match !state with
      | Visited -> ()
      | Visiting ->
        raise_s [%message "combinational loop" ~through_signal:(signal : Signal.t)]
      | Unvisited ->
        state := Visiting;
        List.iter (deps signal) ~f:dfs;
        state := Visited
    in
    (* We only need to check from the given outputs, and all dependents of registers,
       memories etc. *)
    List.iter ~f:dfs (List.concat (t :: List.map initial_visited ~f:deps)))
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
  let new_signal signal = Hashtbl.find_exn new_signal_by_old_uid (uid signal) in
  (* uid generation (note; 1L and up, 0L reserved for empty) *)
  let id = ref 0L in
  let fresh_id () =
    id := Int64.add !id 1L;
    !id
  in
  let new_reg r =
    { reg_clock = new_signal r.reg_clock
    ; reg_clock_edge = r.reg_clock_edge
    ; reg_reset = new_signal r.reg_reset
    ; reg_reset_edge = r.reg_reset_edge
    ; reg_reset_value = new_signal r.reg_reset_value
    ; reg_clear = new_signal r.reg_clear
    ; reg_clear_level = r.reg_clear_level
    ; reg_clear_value = new_signal r.reg_clear_value
    ; reg_enable = new_signal r.reg_enable
    }
  in
  let new_mem m =
    { mem_size = m.mem_size
    ; mem_read_address = new_signal m.mem_read_address
    ; mem_write_address = new_signal m.mem_write_address
    ; mem_write_data = new_signal m.mem_write_data
    }
  in
  let new_write_port write_port =
    { write_clock = new_signal write_port.write_clock
    ; write_address = new_signal write_port.write_address
    ; write_enable = new_signal write_port.write_enable
    ; write_data = new_signal write_port.write_data
    }
  in
  let rec rewrite_signal_upto_wires signal =
    match Hashtbl.find new_signal_by_old_uid (uid signal) with
    | Some x -> x
    | None ->
      let new_deps = List.map (deps signal) ~f:rewrite_signal_upto_wires in
      let update_id id = { id with s_id = fresh_id (); s_deps = new_deps } in
      let new_signal =
        match signal with
        | Empty -> Empty
        | Const { signal_id; constant } ->
          Const
            { signal_id = { signal_id with s_id = fresh_id (); s_deps = [] }; constant }
        | Op2 { signal_id = id; op; arg_a; arg_b } ->
          let arg_a = rewrite_signal_upto_wires arg_a in
          let arg_b = rewrite_signal_upto_wires arg_b in
          Op2
            { signal_id = { id with s_id = fresh_id (); s_deps = [ arg_a; arg_b ] }
            ; op
            ; arg_a
            ; arg_b
            }
        | Mux { signal_id; select; cases } ->
          let select = rewrite_signal_upto_wires select in
          let cases = List.map cases ~f:rewrite_signal_upto_wires in
          Mux
            { signal_id = { signal_id with s_id = fresh_id (); s_deps = select :: cases }
            ; select
            ; cases
            }
        | Cat { signal_id; args } ->
          let args = List.map args ~f:rewrite_signal_upto_wires in
          Cat { signal_id = { signal_id with s_id = fresh_id (); s_deps = args }; args }
        | Not { signal_id; arg } ->
          let arg = rewrite_signal_upto_wires arg in
          Not
            { signal_id = { signal_id with s_id = fresh_id (); s_deps = [ arg ] }; arg }
        | Select { signal_id; arg; high; low } ->
          let arg = rewrite_signal_upto_wires arg in
          Select
            { signal_id = { signal_id with s_id = fresh_id (); s_deps = [ arg ] }
            ; arg
            ; high
            ; low
            }
        | Reg { signal_id; register; d } ->
          let d = rewrite_signal_upto_wires d in
          let register =
            { reg_clock = rewrite_signal_upto_wires register.reg_clock
            ; reg_clock_edge = register.reg_clock_edge
            ; reg_reset = rewrite_signal_upto_wires register.reg_reset
            ; reg_reset_edge = register.reg_reset_edge
            ; reg_reset_value = rewrite_signal_upto_wires register.reg_reset_value
            ; reg_clear = rewrite_signal_upto_wires register.reg_clear
            ; reg_clear_level = register.reg_clear_level
            ; reg_clear_value = rewrite_signal_upto_wires register.reg_clear_value
            ; reg_enable = rewrite_signal_upto_wires register.reg_enable
            }
          in
          Reg
            { signal_id =
                { signal_id with
                  s_id = fresh_id ()
                ; s_deps =
                    [ d
                    ; register.reg_clock
                    ; register.reg_reset
                    ; register.reg_reset_value
                    ; register.reg_clear
                    ; register.reg_clear_value
                    ; register.reg_enable
                    ]
                }
            ; register
            ; d
            }
        | Mem { signal_id; extra_uid = _; register; memory } ->
          Mem
            { signal_id = update_id signal_id
            ; extra_uid = fresh_id ()
            ; register = new_reg register
            ; memory = new_mem memory
            }
        | Multiport_mem { signal_id = id; size; write_ports } ->
          Multiport_mem
            { signal_id = update_id id
            ; size
            ; write_ports = Array.map write_ports ~f:new_write_port
            }
        | Mem_read_port { signal_id = id; memory; read_address } ->
          Mem_read_port
            { signal_id = update_id id
            ; memory = new_signal memory
            ; read_address = new_signal read_address
            }
        | Inst { signal_id; instantiation; _ } ->
          let inputs =
            List.map instantiation.inst_inputs ~f:(fun (name, input) ->
              name, rewrite_signal_upto_wires input)
          in
          Inst
            { signal_id =
                { signal_id with s_id = fresh_id (); s_deps = List.map inputs ~f:snd }
            ; extra_uid = fresh_id ()
            ; instantiation = { instantiation with inst_inputs = inputs }
            }
        | Wire _ -> not_expecting_a_wire signal
      in
      add_mapping ~old_signal:signal ~new_signal;
      new_signal
  in
  (* find wires *)
  let old_wires = filter t ~f:is_wire in
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
  List.map t ~f:new_signal
;;

let fan_out_map ?(deps = Signal.deps) t =
  depth_first_search
    t
    ~init:(Map.empty (module Signal.Uid))
    ~f_before:(fun map signal ->
      let target = Signal.uid signal in
      (* [signal] is in the fan_out of all of its [deps] *)
      List.fold (deps signal) ~init:map ~f:(fun map source ->
        let source = Signal.uid source in
        let fan_out =
          Map.find map source |> Option.value ~default:Signal.Uid_set.empty
        in
        Map.set map ~key:source ~data:(Set.add fan_out target)))
;;

let fan_in_map ?(deps = Signal.deps) t =
  depth_first_search t ~init:Signal.Uid_map.empty ~f_before:(fun map signal ->
    List.map (deps signal) ~f:Signal.uid
    |> Set.of_list (module Signal.Uid)
    |> fun data -> Map.set map ~key:(Signal.uid signal) ~data)
;;

let topological_sort ?(deps = Signal.deps) (graph : t) =
  let module Node = struct
    include Signal

    let compare a b = Signal.Uid.compare (uid a) (uid b)
    let hash s = Signal.Uid.hash (uid s)
    let sexp_of_t = Signal.sexp_of_signal_recursive ~show_uids:false ~depth:0
  end
  in
  let nodes, edges =
    fold graph ~init:([], []) ~f:(fun (nodes, edges) to_ ->
      ( to_ :: nodes
      , List.map (deps to_) ~f:(fun from -> { Topological_sort.Edge.from; to_ })
        @ edges ))
  in
  Topological_sort.sort (module Node) nodes edges |> Or_error.ok_exn
;;

let scheduling_deps (s : Signal.t) =
  match s with
  | Mem { memory; _ } -> [ memory.mem_read_address ]
  | Mem_read_port { read_address; _ } -> [ read_address ]
  | Reg _ -> []
  | Multiport_mem _ -> []
  | Empty | Const _ | Op2 _ | Mux _ | Cat _ | Not _ | Wire _ | Select _ | Inst _ ->
    Signal.deps s
;;

let last_layer_of_nodes ~is_input graph =
  let deps t = scheduling_deps t in
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
      if Signal.is_const signal
      || Signal.is_empty signal
      || Signal.is_multiport_mem signal
      || is_input signal
      then
        Map.set in_layer ~key:(uid signal) ~data:false, false
        (* Regs are not in the final layer either, but we can't add them to the map as
           [false].  We will have to recurse to them each time instead. *)
      else if Signal.is_reg signal
      then in_layer, true
      else (
        (* recurse deeper *)
        let in_layer, is_in_layer = fold_signals (in_layer, false) (deps signal) in
        let is_in_layer = is_in_layer || Signal.is_mem_read_port signal in
        Map.set in_layer ~key:(uid signal) ~data:is_in_layer, is_in_layer)
  (* In final layer if any dependancy is also in the final layer. *)
  and fold_signals layer signals =
    List.fold signals ~init:layer ~f:(fun (in_layer, is_in_layer) signal ->
      let in_layer, is_in_layer' = visit_signal (in_layer, is_in_layer) signal in
      in_layer, is_in_layer || is_in_layer')
  in
  let in_layer, _ = fold_signals (Map.empty (module Signal.Uid), false) graph in
  (* Drop nodes not in the final layer. That will track back to an input or constant but
     not be affected by a register or memory. *)
  Map.to_alist in_layer
  |> List.filter_map ~f:(fun (uid, is_in_layer) ->
    if is_in_layer then Some uid else None)
;;
