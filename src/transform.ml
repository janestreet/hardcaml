open! Import
include Transform_intf
open Signal

type 'a transform_fn' = (Signal.Uid.t -> 'a) -> Signal.t -> 'a
type transform_fn = Signal.t transform_fn'

module type TransformFn' = sig
  type t

  val transform : t transform_fn'
  val rewrite : t transform_fn' -> Signal.t Uid_map.t -> Signal.t list -> t list
  val rewrite_signals : t transform_fn' -> Signal.t list -> t list
end

module type TransformFn = sig
  val transform : transform_fn
end

module MakePureCombTransform (B : MakePureCombTransform_arg) = struct
  type t = B.t

  let transform find signal =
    let find_uid x = uid x |> find in
    let new_signal =
      match signal with
      | Op2 { signal_id = _; op; arg_a; arg_b } ->
        let op2 op = op (find_uid arg_a) (find_uid arg_b) in
        (match op with
         | Signal_add -> op2 B.( +: )
         | Signal_sub -> op2 B.( -: )
         | Signal_mulu -> op2 B.( *: )
         | Signal_muls -> op2 B.( *+ )
         | Signal_and -> op2 B.( &: )
         | Signal_or -> op2 B.( |: )
         | Signal_xor -> op2 B.( ^: )
         | Signal_eq -> op2 B.( ==: )
         | Signal_lt -> op2 B.( <: ))
      | Not { signal_id = _; arg } -> B.( ~: ) (find_uid arg)
      | Cat { signal_id = _; args } -> B.concat_msb (List.map args ~f:find_uid)
      | Mux { signal_id = _; select; cases } ->
        B.mux (find_uid select) (List.map cases ~f:find_uid)
      | Empty -> B.empty
      | Wire { driver; _ } ->
        let w = B.wire (width signal) in
        if not (is_empty !driver) then B.( <== ) w (find_uid !driver);
        w
      | Const { constant; _ } -> B.of_constant (Bits.to_constant constant)
      | Select { arg; high; low; _ } -> B.select (find_uid arg) high low
      | Reg _ -> failwith "MakePureCombTransform: no registers"
      | Mem _ -> failwith "MakePureCombTransform: no memories"
      | Multiport_mem _ -> failwith "MakePureCombTransform: no memories"
      | Mem_read_port _ -> failwith "MakePureCombTransform: no memories"
      | Inst _ -> failwith "MakePureCombTransform: no instantiations"
    in
    (* apply names *)
    if B.is_empty new_signal
    then new_signal
    else List.fold (names signal) ~init:new_signal ~f:(fun s n -> B.( -- ) s n)
  ;;

  let copy_names s t = List.fold (names s) ~init:t ~f:(fun t n -> B.( -- ) t n)

  let rewrite (fn : B.t transform_fn') id_to_sig outputs =
    let idv k _ = k in
    let set_of_map f map =
      Map.fold
        map
        ~init:(Set.empty (module Uid))
        ~f:(fun ~key:k ~data:v s -> Set.add s (f k v))
    in
    let set_of_list f _ =
      List.fold [] ~init:(Set.empty (module Uid)) ~f:(fun s v -> Set.add s (f v))
    in
    let partition compare set =
      Set.fold
        set
        ~init:(Set.empty (module Uid), Set.empty (module Uid))
        ~f:(fun (tr, fl) k -> if compare k then Set.add tr k, fl else tr, Set.add fl k)
    in
    let find uid = Map.find_exn id_to_sig uid in
    (*let partition_const = partition (find >> is_const) in*)
    let partition_wire = partition (fun x -> find x |> is_wire) in
    let partition_ready ready remaining =
      let ready s =
        let s = find s in
        (* look up the signal *)
        let dep_set = set_of_list uid (deps s) in
        Set.is_subset dep_set ~of_:ready
      in
      let new_ready, not_ready = partition ready remaining in
      if Set.length new_ready = 0
      then failwith "Could not schedule anything"
      else new_ready, not_ready
    in
    (* {[
         let all_set = set_of_map idv id_to_sig in
         let const_set, remaining_set = partition_const all_set in
         let wire_set, remaining_set = partition_wire remaining_set in
         let ready_set = UidSet.union wire_set const_set in
       ]} *)
    let all_set = set_of_map idv id_to_sig in
    let wire_set, remaining_set = partition_wire all_set in
    let ready_set = wire_set in
    (* Copy the wires and constants.  We potentially dont need to do this for the
       constants, but the wires must be done this way to break combinatorial
       dependancies. *)
    let map : B.t Uid_map.t =
      Set.fold ready_set ~init:Uid_map.empty ~f:(fun map uid ->
        let signal = find uid in
        match signal with
        | Wire _ ->
          Map.set map ~key:uid ~data:(copy_names signal (B.wire (width signal)))
        (*| Const _ -> Map.set uid signal map*)
        | _ -> failwith "unexpected signal")
    in
    (* now recursively rewrite nodes as they become ready *)
    let rec rewrite map ready remaining =
      if Set.length remaining = 0
      then map
      else (
        let find_new map uid = Map.find_exn map uid in
        let new_ready, new_remaining = partition_ready ready remaining in
        (* rewrite the ready nodes *)
        let map =
          Set.fold new_ready ~init:map ~f:(fun map uid ->
            let old_signal = find uid in
            let new_signal = fn (find_new map) old_signal in
            Map.set map ~key:uid ~data:new_signal)
        in
        rewrite map (Set.union ready new_ready) new_remaining)
    in
    let map = rewrite map ready_set remaining_set in
    (* reattach all wires *)
    Set.iter wire_set ~f:(fun uid' ->
      let o = Map.find_exn id_to_sig uid' in
      let n = Map.find_exn map uid' in
      match o with
      | Wire { driver; _ } ->
        if not (is_empty !driver)
        then (
          let d = Map.find_exn map (uid !driver) in
          B.(n <== d))
      | _ -> failwith "expecting a wire");
    (* find new outputs *)
    let outputs = List.map outputs ~f:(fun signal -> Map.find_exn map (uid signal)) in
    outputs
  ;;

  let rewrite_signals fn signals =
    let id_to_sig =
      Signal_graph.depth_first_search
        (Signal_graph.create signals)
        ~init:Uid_map.empty
        ~f_before:(fun map signal -> Map.add_exn map ~key:(uid signal) ~data:signal)
    in
    rewrite fn id_to_sig signals
  ;;
end

module MakeCombTransform (B : Comb.Primitives with type t = Signal.t) = struct
  let transform find signal =
    let find_uid x = uid x |> find in
    let new_signal =
      match signal with
      | Op2 { signal_id = _; op; arg_a; arg_b } ->
        let op2 op = op (find_uid arg_a) (find_uid arg_b) in
        (match op with
         | Signal_add -> op2 B.( +: )
         | Signal_sub -> op2 B.( -: )
         | Signal_mulu -> op2 B.( *: )
         | Signal_muls -> op2 B.( *+ )
         | Signal_and -> op2 B.( &: )
         | Signal_or -> op2 B.( |: )
         | Signal_xor -> op2 B.( ^: )
         | Signal_eq -> op2 B.( ==: )
         | Signal_lt -> op2 B.( <: ))
      | Not { signal_id = _; arg } -> B.( ~: ) (find_uid arg)
      | Cat { signal_id = _; args } -> B.concat_msb (List.map args ~f:find_uid)
      | Mux { signal_id = _; select; cases } ->
        B.mux (find_uid select) (List.map cases ~f:find_uid)
      | Empty -> B.empty
      | Wire { driver; _ } ->
        let w = Signal.wire (width signal) in
        if not (is_empty !driver) then Signal.( <== ) w (find_uid !driver);
        w
      | Const { constant; _ } -> B.of_constant (Bits.to_constant constant)
      | Select { arg; high; low; _ } -> B.select (find_uid arg) high low
      | Reg { register; d; _ } ->
        reg
          (Reg_spec.override
             (Reg_spec.create ~clock:(find_uid register.reg_clock) ())
             ~reset_edge:register.reg_reset_edge
             ~clear_level:register.reg_clear_level
             ~reset:(find_uid register.reg_reset)
             ~reset_to:(find_uid register.reg_reset_value)
             ~clear:(find_uid register.reg_clear)
             ~clear_to:(find_uid register.reg_clear_value))
          ~enable:(find_uid register.reg_enable)
          (find_uid d)
      | Mem { register = r; memory = m; _ } ->
        memory
          ~write_port:
            { write_clock = find_uid r.reg_clock
            ; write_enable = find_uid r.reg_enable
            ; write_address = find_uid m.mem_write_address
            ; write_data = find_uid m.mem_write_data
            }
          ~read_address:(find_uid m.mem_read_address)
          m.mem_size
      | Multiport_mem _ ->
        failwith "Transform Multiport_mem"
      | Mem_read_port _ ->
        failwith "Transform Mem_read_port"
      | Inst { instantiation; _ } ->
        let inputs =
          List.map instantiation.inst_inputs ~f:(fun (name, input) ->
            name, find (uid input))
        in
        Inst
          { signal_id = make_id (width signal) (List.map inputs ~f:snd)
          ; extra_uid = new_id ()
          ; instantiation = { instantiation with inst_inputs = inputs }
          }
    in
    (* apply names *)
    if B.is_empty new_signal
    then new_signal
    else List.fold (names signal) ~init:new_signal ~f:(fun s n -> s -- n)
  ;;
end

module CopyTransform = MakeCombTransform (Signal)

let copy_names s t = List.fold (names s) ~init:t ~f:(fun t n -> t -- n)

let rewrite fn id_to_sig outputs =
  let idv k _ = k in
  let set_of_map f map =
    Map.fold
      map
      ~init:(Set.empty (module Uid))
      ~f:(fun ~key ~data s -> Set.add s (f key data))
  in
  let set_of_list f _ =
    List.fold [] ~init:(Set.empty (module Uid)) ~f:(fun s v -> Set.add s (f v))
  in
  let partition compare set =
    Set.fold
      set
      ~init:(Set.empty (module Uid), Set.empty (module Uid))
      ~f:(fun (tr, fl) k -> if compare k then Set.add tr k, fl else tr, Set.add fl k)
  in
  let find uid = Map.find_exn id_to_sig uid in
  (*let partition_const = partition (find >> is_const) in*)
  let partition_wire = partition (fun x -> find x |> is_wire) in
  let partition_ready ready remaining =
    let ready s =
      let s = find s in
      (* look up the signal *)
      let dep_set = set_of_list uid (deps s) in
      Set.is_subset dep_set ~of_:ready
    in
    let new_ready, not_ready = partition ready remaining in
    if Set.length new_ready = 0
    then failwith "Could not schedule anything"
    else new_ready, not_ready
  in
  (* {[
       let all_set = set_of_map idv id_to_sig in
       let const_set, remaining_set = partition_const all_set in
       let wire_set, remaining_set = partition_wire remaining_set in
       let ready_set = Set.union wire_set const_set in
     ]} *)
  let all_set = set_of_map idv id_to_sig in
  let wire_set, remaining_set = partition_wire all_set in
  let ready_set = wire_set in
  (* Copy the wires and constants.  We potentially dont need to do this for the constants,
     but the wires must be done this way to break combinatorial dependancies. *)
  let map =
    Set.fold ready_set ~init:Uid_map.empty ~f:(fun map uid ->
      let signal = find uid in
      match signal with
      | Wire _ -> Map.set map ~key:uid ~data:(copy_names signal (wire (width signal)))
      (*| Const _ -> Map.set uid signal map*)
      | _ -> failwith "unexpected signal")
  in
  (* now recursively rewrite nodes as they become ready *)
  let rec rewrite map ready remaining =
    if Set.length remaining = 0
    then map
    else (
      let find_new map uid = Map.find_exn map uid in
      let new_ready, new_remaining = partition_ready ready remaining in
      (* rewrite the ready nodes *)
      let map =
        Set.fold new_ready ~init:map ~f:(fun map uid ->
          let old_signal = find uid in
          let new_signal = fn (find_new map) old_signal in
          Map.set map ~key:uid ~data:new_signal)
      in
      rewrite map (Set.union ready new_ready) new_remaining)
  in
  let map = rewrite map ready_set remaining_set in
  (* reattach all wires *)
  Set.iter wire_set ~f:(fun uid' ->
    let o = Map.find_exn id_to_sig uid' in
    let n = Map.find_exn map uid' in
    match o with
    | Wire { driver; _ } ->
      if not (is_empty !driver)
      then (
        let d = Map.find_exn map (uid !driver) in
        n <== d)
    | _ -> failwith "expecting a wire");
  (* find new outputs *)
  let outputs = List.map outputs ~f:(fun signal -> Map.find_exn map (uid signal)) in
  outputs
;;

let rewrite_signals fn signals =
  let id_to_sig =
    Signal_graph.depth_first_search
      (Signal_graph.create signals)
      ~init:Uid_map.empty
      ~f_before:(fun map signal -> Map.add_exn map ~key:(uid signal) ~data:signal)
  in
  rewrite fn id_to_sig signals
;;
