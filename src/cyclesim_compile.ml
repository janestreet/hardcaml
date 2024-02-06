[@@@ocaml.flambda_o3]

open Base
open! Cyclesim0
module Schedule = Cyclesim_schedule

let uid = Signal.uid
let names = Signal.names

(* Find all nodes we wish to trace, and generate unique names. *)
module Traced_nodes = struct
  let create_mangler circuit =
    let mangler = Mangler.create ~case_sensitive:true in
    let io_port_names =
      Circuit.inputs circuit @ Circuit.outputs circuit
      |> List.map ~f:(fun s -> Signal.names s |> List.hd_exn)
    in
    Mangler.add_identifiers_exn mangler io_port_names;
    mangler
  ;;

  let internal_signal mangler signal =
    let mangled_names = Signal.names signal |> List.map ~f:(Mangler.mangle mangler) in
    { Traced.signal; mangled_names }
  ;;

  let io_port signal =
    let name = Signal.names signal |> List.hd_exn in
    { Traced.signal; name }
  ;;

  let create circuit ~is_internal_port =
    let internal_signals =
      match is_internal_port with
      | None -> []
      | Some f ->
        let mangler = create_mangler circuit in
        Signal_graph.filter (Circuit.signal_graph circuit) ~f:(fun s ->
          (not (Circuit.is_input circuit s))
          && (not (Circuit.is_output circuit s))
          && (not (Signal.is_empty s))
          && f s)
        |> List.rev
        |> List.map ~f:(internal_signal mangler)
    in
    { Traced.input_ports = List.map (Circuit.inputs circuit) ~f:io_port
    ; output_ports = List.map (Circuit.outputs circuit) ~f:io_port
    ; internal_signals
    }
  ;;
end

module Maps : sig
  type t

  val create : traced:Cyclesim0.Traced.internal_signal list -> Schedule.t -> t
  val find_data : t -> Signal.t -> Bits.Mutable.t option
  val find_data_exn : t -> Signal.t -> Bits.Mutable.t
  val find_data_or_empty : t -> Signal.t -> Bits.Mutable.t
  val find_reg_exn : t -> Signal.t -> Bits.Mutable.t
  val find_mem : t -> Signal.t -> Bits.Mutable.t array option
  val find_mem_exn : t -> Signal.t -> Bits.Mutable.t array
end = struct
  type t =
    { data : Bits.Mutable.t Map.M(Signal.Uid).t
    ; reg : Bits.Mutable.t Map.M(Signal.Uid).t
    ; mem : Bits.Mutable.t array Map.M(Signal.Uid).t
    ; name_to_uid : Signal.Uid.t Map.M(String).t
    ; aliases : Schedule.Aliases.t
    }

  let uid = Signal.uid

  let alias_or_self (t : t) uid =
    match Schedule.Aliases.resolve_alias t.aliases uid with
    | None -> uid
    | Some alias -> alias
  ;;

  let create_name_to_uid traced =
    List.concat_map traced ~f:(fun { Cyclesim0.Traced.signal; mangled_names } ->
      let uid = Signal.uid signal in
      List.map mangled_names ~f:(fun name -> name, uid))
    |> Map.of_alist_exn (module String)
  ;;

  let create ~traced (bundle : Schedule.t) =
    let aliases = Schedule.aliases bundle in
    let zero n = Bits.Mutable.of_constant (Constant.of_int ~width:n 0) in
    let const c = Bits.Mutable.of_constant (Bits.to_constant c) in
    let data_map =
      let data_add map signal =
        if not (Schedule.Aliases.is_alias aliases (uid signal))
        then (
          let value =
            match (signal : Signal.t) with
            | Const { constant; _ } -> const constant
            | _ -> zero (Signal.width signal)
          in
          Map.set map ~key:(uid signal) ~data:value)
        else map
      in
      let add_data_from_list list map = List.fold list ~init:map ~f:data_add in
      Map.empty (module Signal.Uid)
      |> add_data_from_list (Schedule.regs bundle)
      |> add_data_from_list (Schedule.inputs bundle)
      |> add_data_from_list (Schedule.consts bundle)
      |> add_data_from_list (Schedule.schedule bundle)
    in
    let reg_map =
      let reg_add map signal =
        Map.set map ~key:(uid signal) ~data:(zero (Signal.width signal))
      in
      List.fold (Schedule.regs bundle) ~init:(Map.empty (module Signal.Uid)) ~f:reg_add
    in
    let mem_map =
      let mem_add map (signal : Signal.t) =
        match signal with
        | Multiport_mem { size = mem_size; _ } ->
          let mem = Array.init mem_size ~f:(fun _ -> zero (Signal.width signal)) in
          Map.set map ~key:(uid signal) ~data:mem
        | _ -> raise_s [%message "Expecting memory"]
      in
      List.fold (Schedule.mems bundle) ~init:(Map.empty (module Signal.Uid)) ~f:mem_add
    in
    let name_to_uid = create_name_to_uid traced in
    { data = data_map; reg = reg_map; mem = mem_map; aliases; name_to_uid }
  ;;

  let find_data_by_uid maps uid = Map.find maps.data (alias_or_self maps uid)
  let find_data_by_uid_exn maps uid = Map.find_exn maps.data (alias_or_self maps uid)
  let find_data maps signal = find_data_by_uid maps (uid signal)
  let find_data_exn maps signal = find_data_by_uid_exn maps (uid signal)

  let find_data_or_empty maps signal =
    match Map.find maps.data (alias_or_self maps (uid signal)) with
    | None -> Bits.Mutable.empty
    | Some bits -> bits
  ;;

  let find_reg_by_uid_exn maps uid = Map.find_exn maps.reg uid
  let find_reg_exn maps signal = find_reg_by_uid_exn maps (uid signal)
  let find_mem maps signal = Map.find maps.mem (uid signal)
  let find_mem_exn maps signal = Map.find_exn maps.mem (uid signal)
end

module Io_ports = struct
  type 'a t =
    { in_ports : 'a
    ; out_ports : 'a
    }

  let create circuit maps : _ t =
    (* list of input ports *)
    let in_ports =
      List.map (Circuit.inputs circuit) ~f:(fun signal ->
        List.hd_exn (names signal), Maps.find_data_exn maps signal)
    in
    (* list of output ports *)
    let out_ports =
      List.map (Circuit.outputs circuit) ~f:(fun signal ->
        List.hd_exn (names signal), Maps.find_data_exn maps signal)
    in
    { in_ports; out_ports }
  ;;

  let[@cold] raise_input_port_width_mismatch port_name src dst =
    let got_width = Bits.width src in
    let expected_width = Bits.Mutable.width dst in
    raise_s
      [%message
        "Simulation input width mismatch"
          (port_name : string)
          (src : Bits.t)
          (expected_width : int)
          (got_width : int)]
  ;;

  let check_input (maps : Maps.t) signal =
    let signal_width = Signal.width signal in
    let name = List.hd_exn (names signal) in
    let tgt = Maps.find_data_exn maps signal in
    let check_input () =
      let data_width = Bits.Mutable.width tgt in
      if data_width <> signal_width
      then
        raise_s
          [%message
            "Input port assigned invalid width"
              name
              (data_width : int)
              (signal_width : int)]
    in
    check_input
  ;;
end

module Compile = struct
  let mutable_to_int x = Int64.to_int_trunc (Bits.Mutable.unsafe_get_int64 x 0)

  let dispach_on_width width ~less_than_64 ~exactly_64 ~more_than_64 =
    Some
      (if width <= 64
       then less_than_64
       else if width = 64
       then exactly_64
       else more_than_64)
  ;;

  let[@inline] set0 t value = Bits.Mutable.unsafe_set_int64 t 0 value
  let[@inline] get0 t = Bits.Mutable.unsafe_get_int64 t 0

  let[@inline] mask ~width x =
    let mask = Int64.( lsr ) (-1L) (64 - width) in
    Int64.( land ) mask x
  ;;

  (* Obj.magic is marginally faster, but not enough to live with the code smell. *)
  let[@inline] int64_of_bool (x : bool) : int64 = if x then 1L else 0L

  let signal ~aliases ~combinational_ops_database (maps : Maps.t) signal =
    let find_exn signal = Maps.find_data_exn maps signal in
    let find_or_empty signal = Maps.find_data_or_empty maps signal in
    let tgt = find_exn signal in
    let deps = Signal.Type.Deps.map signal ~f:find_or_empty in
    match signal with
    | Empty -> raise_s [%message "Can't compile empty signal"]
    | Const _ -> None
    | Not { arg; _ } ->
      let arg = find_or_empty arg in
      let notsmall () =
        let width = Bits.Mutable.width arg in
        set0 tgt (mask ~width (Int64.( lxor ) (get0 arg) (-1L)))
      in
      let not64 () = set0 tgt (Int64.( lxor ) (get0 arg) (-1L)) in
      let notbig () = Bits.Mutable.( ~: ) tgt arg in
      dispach_on_width
        (Bits.Mutable.width tgt)
        ~less_than_64:notsmall
        ~exactly_64:not64
        ~more_than_64:notbig
    | Cat { args; _ } ->
      let args = List.map args ~f:find_or_empty in
      let cat64 =
        let args = Array.of_list args in
        fun () ->
          let acc = ref 0L in
          for i = 0 to Array.length args - 1 do
            let arg = Array.unsafe_get args i in
            let arg_width = Bits.Mutable.width arg in
            acc := Int64.O.(get0 arg lor (!acc lsl arg_width))
          done;
          set0 tgt !acc
      in
      let catbig =
        let args = Array.of_list args in
        Array.rev_inplace args;
        fun () -> Bits.Mutable.concat_rev_array tgt args
      in
      dispach_on_width
        (Bits.Mutable.width tgt)
        ~less_than_64:cat64
        ~exactly_64:cat64
        ~more_than_64:catbig
    | Mux { select; cases; _ } ->
      let sel = find_or_empty select in
      let els = Array.of_list (List.map cases ~f:find_or_empty) in
      let muxn () =
        let sel = mutable_to_int sel in
        let max = Array.length els - 1 in
        let sel = if sel > max then max else sel in
        Bits.Mutable.copy ~dst:tgt ~src:(Array.unsafe_get els sel)
      in
      let mux64 () =
        let sel = mutable_to_int sel in
        let max = Array.length els - 1 in
        let sel = if sel > max then max else sel in
        let (selected : Bits.Mutable.t) = Array.unsafe_get els sel in
        set0 tgt (get0 selected)
      in
      dispach_on_width
        (Bits.Mutable.width tgt)
        ~less_than_64:mux64
        ~exactly_64:mux64
        ~more_than_64:muxn
    | Op2 { op; arg_a; arg_b; signal_id = _ } ->
      let a = find_or_empty arg_a in
      let b = find_or_empty arg_b in
      (match op with
       | Signal_add ->
         let addsmall () =
           set0 tgt (mask ~width:(Bits.Mutable.width a) (Int64.( + ) (get0 a) (get0 b)))
         in
         let add64 () = set0 tgt (Int64.( + ) (get0 a) (get0 b)) in
         let addbig () = Bits.Mutable.( +: ) tgt a b in
         dispach_on_width
           (Bits.Mutable.width tgt)
           ~less_than_64:addsmall
           ~exactly_64:add64
           ~more_than_64:addbig
       | Signal_sub ->
         let subsmall () =
           set0 tgt (mask ~width:(Bits.Mutable.width a) (Int64.( - ) (get0 a) (get0 b)))
         in
         let sub64 () = set0 tgt (Int64.( - ) (get0 a) (get0 b)) in
         let subbig () = Bits.Mutable.( -: ) tgt a b in
         dispach_on_width
           (Bits.Mutable.width tgt)
           ~less_than_64:subsmall
           ~exactly_64:sub64
           ~more_than_64:subbig
       | Signal_mulu ->
         let mulu () = Bits.Mutable.( *: ) tgt a b in
         Some mulu
       | Signal_muls ->
         let muls () = Bits.Mutable.( *+ ) tgt a b in
         Some muls
       | Signal_and ->
         let and64 () = set0 tgt (Int64.( land ) (get0 a) (get0 b)) in
         let andbig () = Bits.Mutable.( &: ) tgt a b in
         dispach_on_width
           (Bits.Mutable.width tgt)
           ~less_than_64:and64
           ~exactly_64:and64
           ~more_than_64:andbig
       | Signal_or ->
         let or64 () = set0 tgt (Int64.( lor ) (get0 a) (get0 b)) in
         let orbig () = Bits.Mutable.( |: ) tgt a b in
         dispach_on_width
           (Bits.Mutable.width tgt)
           ~less_than_64:or64
           ~exactly_64:or64
           ~more_than_64:orbig
       | Signal_xor ->
         let xor64 () = set0 tgt (Int64.( lxor ) (get0 a) (get0 b)) in
         let xorbig () = Bits.Mutable.( ^: ) tgt a b in
         dispach_on_width
           (Bits.Mutable.width tgt)
           ~less_than_64:xor64
           ~exactly_64:xor64
           ~more_than_64:xorbig
       | Signal_eq ->
         let eq64 () = set0 tgt (int64_of_bool (Int64.( = ) (get0 a) (get0 b))) in
         let eqbig () = Bits.Mutable.( ==: ) tgt a b in
         dispach_on_width
           (Bits.Mutable.width a)
           ~less_than_64:eq64
           ~exactly_64:eq64
           ~more_than_64:eqbig
       | Signal_lt ->
         let ltsmall () = set0 tgt (int64_of_bool (Int64.( < ) (get0 a) (get0 b))) in
         let lt64 () =
           set0 tgt (int64_of_bool (Stdlib.Int64.unsigned_compare (get0 a) (get0 b) = -1))
         in
         let ltbig () = Bits.Mutable.( <: ) tgt a b in
         let width = Bits.Mutable.width a in
         if width <= 63
         then Some ltsmall
         else if width = 64
         then Some lt64
         else Some ltbig)
    | Wire _ ->
      if not (Schedule.Aliases.is_alias aliases (Signal.uid signal))
      then raise_s [%message "Expecting all non-input wires to be compiled into aliases"];
      None
    | Select { arg; high; low; _ } ->
      let d = find_or_empty arg in
      let selectsmall () =
        let width = Bits.Mutable.width tgt in
        let x = Int64.( lsr ) (get0 d) low in
        let x = mask ~width x in
        set0 tgt x
      in
      let select64from64 () = set0 tgt (get0 d) in
      let selectbig () = Bits.Mutable.select tgt d high low in
      dispach_on_width
        (Signal.width arg)
        ~less_than_64:selectsmall
        ~exactly_64:
          (if high - low + 1 = 64
           then (* Weird case where arg is 64 bits and dst is 64 bits  *)
             select64from64
           else selectsmall)
        ~more_than_64:selectbig
    | Reg { register = r; d; _ } ->
      let tgt = Maps.find_reg_exn maps signal in
      let src = find_or_empty d in
      let clr =
        if not (Signal.is_empty r.reg_clear)
        then
          Some
            ( Maps.find_data_exn maps r.reg_clear
            , Maps.find_data_exn maps r.reg_clear_value
            , Level.to_int r.reg_clear_level )
        else None
      in
      let ena =
        if not (Signal.is_empty r.reg_enable)
        then Some (Maps.find_data_exn maps r.reg_enable)
        else None
      in
      (match clr, ena with
       | None, None ->
         let reg () = Bits.Mutable.copy ~dst:tgt ~src in
         Some reg
       | Some (c, v, l), None ->
         let regc () =
           if mutable_to_int c = l
           then Bits.Mutable.copy ~dst:tgt ~src:v
           else Bits.Mutable.copy ~dst:tgt ~src
         in
         Some regc
       | None, Some e ->
         let rege () = if mutable_to_int e = 1 then Bits.Mutable.copy ~dst:tgt ~src in
         Some rege
       | Some (c, v, l), Some e ->
         let regce () =
           if mutable_to_int c = l
           then Bits.Mutable.copy ~dst:tgt ~src:v
           else if mutable_to_int e = 1
           then Bits.Mutable.copy ~dst:tgt ~src
         in
         Some regce)
    | Multiport_mem _ -> None
    | Mem_read_port { memory; read_address; _ } ->
      let mem = Maps.find_mem_exn maps memory in
      let addr = Maps.find_data_exn maps read_address in
      let zero = Bits.Mutable.create (Signal.width signal) in
      let mem_read () =
        let data =
          let addr = mutable_to_int addr in
          if 0 <= addr && addr < Array.length mem then Array.unsafe_get mem addr else zero
        in
        Bits.Mutable.copy ~dst:tgt ~src:data
      in
      Some mem_read
    | Inst { instantiation = i; _ } ->
      (match
         Combinational_ops_database.find combinational_ops_database ~name:i.inst_name
       with
       | None ->
         raise_s
           [%message
             "Instantiation not supported in simulation" ~name:(i.inst_name : string)]
       | Some op ->
         let f = Combinational_op.create_fn op in
         let outputs =
           List.map i.inst_outputs ~f:(fun (_, (b, _)) -> Bits.Mutable.create b)
         in
         let inst () =
           f deps outputs;
           Bits.Mutable.concat tgt (List.rev outputs)
         in
         Some inst)
  ;;

  let register_update (maps : Maps.t) (signals : Signal.t list) =
    let signals = Array.of_list signals in
    let single_word_signals, multiple_word_signals =
      Array.partition_tf signals ~f:(fun signal -> Signal.width signal <= 64)
    in
    let entries_of_signals (signals : Signal.t array) =
      let dsts =
        Array.map signals ~f:(fun signal ->
          match signal with
          | Reg _ ->
            let dst = Maps.find_data_exn maps signal in
            dst
          | _ -> raise_s [%message "[compile_reg_update] expecting register"])
      in
      let srcs =
        Array.map signals ~f:(fun signal ->
          match signal with
          | Reg _ ->
            let src = Maps.find_reg_exn maps signal in
            src
          | _ -> raise_s [%message "[compile_reg_update] expecting register"])
      in
      dsts, srcs
    in
    let single_word_dsts, single_word_srcs = entries_of_signals single_word_signals in
    let multiple_word_dsts, multiple_word_srcs =
      entries_of_signals multiple_word_signals
    in
    let reg_updates () =
      let () =
        let len = Array.length single_word_dsts in
        for i = 0 to len - 1 do
          let src0 = Array.unsafe_get single_word_srcs i in
          let dst0 = Array.unsafe_get single_word_dsts i in
          Bits.Mutable.unsafe_set_int64 dst0 0 (Bits.Mutable.unsafe_get_int64 src0 0)
        done
      in
      for i = 0 to Array.length multiple_word_dsts - 1 do
        let src = Array.unsafe_get multiple_word_srcs i in
        let dst = Array.unsafe_get multiple_word_dsts i in
        let words = Bits.Mutable.num_words src in
        for j = 0 to words - 1 do
          Bits.Mutable.unsafe_set_int64 dst j (Bits.Mutable.unsafe_get_int64 src j)
        done
      done
    in
    Staged.stage reg_updates
  ;;

  let memory_update (maps : Maps.t) (signal : Signal.t) =
    match signal with
    | Multiport_mem { size; write_ports; _ } ->
      let mem = Maps.find_mem_exn maps signal in
      let multi_mem_wr (write_port : _ Write_port.t) =
        let we = Maps.find_data_exn maps write_port.write_enable in
        let w = Maps.find_data_exn maps write_port.write_address in
        let d = Maps.find_data_exn maps write_port.write_data in
        let multi_mem_wr () =
          if mutable_to_int we = 1
          then (
            let w = mutable_to_int w in
            if w >= size then () else Bits.Mutable.copy ~dst:mem.(w) ~src:d)
        in
        multi_mem_wr
      in
      let write_ports = Array.map write_ports ~f:multi_mem_wr in
      let multi_mem_wr_ports () = Array.iter write_ports ~f:(fun f -> f ()) in
      multi_mem_wr_ports
    | _ -> raise_s [%message "[Compile.memory_update] expecting memory"]
  ;;

  let reset (maps : Maps.t) (signal : Signal.t) =
    match signal with
    | Reg { register = r; _ } ->
      if not (Signal.is_empty r.reg_reset)
      then (
        let tgt0 = Maps.find_data_exn maps signal in
        let tgt1 = Maps.find_reg_exn maps signal in
        let value = Maps.find_data_exn maps r.reg_reset_value in
        let reg_reset () =
          Bits.Mutable.copy ~dst:tgt0 ~src:value;
          Bits.Mutable.copy ~dst:tgt1 ~src:value
        in
        Some reg_reset)
      else None
    | _ -> raise_s [%message "[Compile.reset] only registers should have a reset"]
  ;;
end

module Last_layer = struct
  let create ~aliases circuit tasks_comb =
    (* Find nodes between registers and outputs *)
    let nodes =
      Signal_graph.last_layer_of_nodes
        ~is_input:(Circuit.is_input circuit)
        (Circuit.signal_graph circuit)
      |> List.filter ~f:(fun uid -> not (Schedule.Aliases.is_alias aliases uid))
      |> Set.of_list (module Signal.Uid)
    in
    (* Compile them *)
    let tasks =
      List.filter_map tasks_comb ~f:(fun (uid, t) ->
        if Set.mem nodes uid then Some (uid, t) else None)
    in
    let nodes' = Set.of_list (module Signal.Uid) (List.map tasks ~f:fst) in
    (* Check that all signals in the last layer could be compiled. *)
    if Set.equal nodes nodes'
    then List.map tasks ~f:snd
    else (
      let diff = Set.diff nodes nodes' |> Set.to_list in
      let diff =
        List.map diff ~f:(fun uid ->
          ( uid
          , try Some (Circuit.find_signal_exn circuit uid) with
            | _ -> None ))
      in
      raise_s
        [%message
          "[Cyclesim.create] Last layer did not compile correctly"
            (diff : (Signal.Uid.t * Signal.t option) list)])
  ;;
end

let create ?(config = Cyclesim0.Config.default) circuit =
  let circuit =
    if config.deduplicate_signals then Dedup.deduplicate circuit else circuit
  in
  (* add internally traced nodes *)
  let traced = Traced_nodes.create circuit ~is_internal_port:config.is_internal_port in
  let bundle = Schedule.create circuit in
  let aliases = Schedule.aliases bundle in
  (* build maps *)
  let maps : Maps.t = Maps.create ~traced:traced.internal_signals bundle in
  let compile =
    Compile.signal
      ~combinational_ops_database:config.combinational_ops_database
      ~aliases
      maps
  in
  let compile_and_tag s = Option.map (compile s) ~f:(fun t -> uid s, t) in
  (* compile the task list *)
  let tasks_check = List.map (Schedule.inputs bundle) ~f:(Io_ports.check_input maps) in
  let tasks_comb =
    List.filter_opt (List.map (Schedule.schedule bundle) ~f:compile_and_tag)
  in
  let tasks_regs = List.filter_opt (List.map (Schedule.regs bundle) ~f:compile) in
  let tasks_at_clock_edge =
    List.concat
      [ List.map (Schedule.mems bundle) ~f:(Compile.memory_update maps)
      ; [ Staged.unstage (Compile.register_update maps (Schedule.regs bundle)) ]
      ]
  in
  (* reset *)
  let resets =
    List.filter_opt (List.map (Schedule.regs bundle) ~f:(Compile.reset maps))
  in
  let { Io_ports.in_ports; out_ports } = Io_ports.create circuit maps in
  (* simulator structure *)
  let task tasks =
    let tasks = Array.of_list tasks in
    fun () ->
      for i = 0 to Array.length tasks - 1 do
        (Array.unsafe_get tasks i) ()
      done
  in
  let tasks tasks = task (List.concat tasks) in
  let in_ports, in_ports_task =
    let i =
      List.map in_ports ~f:(fun (n, b) -> n, ref (Bits.zero (Bits.Mutable.width b)))
    in
    let names = Array.of_list (List.map i ~f:fst) in
    let src_and_tgt =
      List.map2_exn in_ports i ~f:(fun (_, tgt) (_, src) -> src, tgt) |> Array.of_list
    in
    let copy_in_ports () =
      for i = 0 to Array.length src_and_tgt - 1 do
        let src, tgt = Array.unsafe_get src_and_tgt i in
        if Bits.width !src <> Bits.Mutable.width tgt
        then Io_ports.raise_input_port_width_mismatch names.(i) !src tgt;
        Bits.Mutable.copy_bits ~dst:tgt ~src:!src
      done
    in
    i, copy_in_ports
  in
  let out_ports_ref ports =
    let p = List.map ports ~f:(fun (n, s) -> n, ref (Bits.zero (Bits.Mutable.width s))) in
    let tgt_and_src =
      List.map2_exn p ports ~f:(fun (_, rf) (_, v) -> rf, v) |> Array.of_list
    in
    let copy_out_ports () =
      for i = 0 to Array.length tgt_and_src - 1 do
        let tgt, src = Array.unsafe_get tgt_and_src i in
        tgt := Bits.Mutable.to_bits src
      done
    in
    p, copy_out_ports
  in
  let out_ports_before_clock_edge, out_ports_before_clock_edge_task =
    out_ports_ref out_ports
  in
  let out_ports_after_clock_edge, out_ports_task = out_ports_ref out_ports in
  let tasks_before_clock_edge = List.map tasks_comb ~f:snd in
  let tasks_after_clock_edge =
    let optimize_last_layer = true in
    if optimize_last_layer
    then Last_layer.create ~aliases circuit tasks_comb
    else tasks_before_clock_edge
  in
  let lookup_node (t : Traced.internal_signal) =
    if Signal.Type.is_mem t.signal || Signal.Type.is_reg t.signal
    then None
    else
      Maps.find_data maps t.signal
      |> Option.map ~f:Cyclesim_lookup.Node.create_from_bits_mutable
  in
  let lookup_reg (t : Traced.internal_signal) =
    if not (Signal.Type.is_reg t.signal)
    then None
    else
      Maps.find_data maps t.signal
      |> Option.map ~f:Cyclesim_lookup.Reg.create_from_bits_mutable
  in
  let lookup_mem (t : Traced.internal_signal) =
    if not (Signal.Type.is_mem t.signal)
    then None
    else
      Maps.find_mem maps t.signal
      |> Option.map ~f:Cyclesim_lookup.Memory.create_from_bits_mutable_array
  in
  Cyclesim0.Private.create
    ~in_ports
    ~out_ports_after_clock_edge
    ~out_ports_before_clock_edge
    ~traced
    ~lookup_node
    ~lookup_reg
    ~lookup_mem
    ~cycle_check:(task tasks_check)
    ~cycle_before_clock_edge:
      (tasks
         [ [ in_ports_task ]
         ; tasks_before_clock_edge
         ; tasks_regs
         ; [ out_ports_before_clock_edge_task ]
         ])
    ~cycle_at_clock_edge:(task tasks_at_clock_edge)
    ~cycle_after_clock_edge:(tasks [ tasks_after_clock_edge; [ out_ports_task ] ])
    ~reset:(task resets)
    ?circuit:(if config.store_circuit then Some circuit else None)
    ()
;;
