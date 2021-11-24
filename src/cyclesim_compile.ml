open Base
open! Cyclesim0
module Schedule = Cyclesim_schedule

let uid = Signal.uid
let names = Signal.names

module Mangle_names = struct
  module IntPair = struct
    module T = struct
      type t = Signal.Uid.t * int [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  let port_names_map ports =
    List.fold
      ports
      ~init:(Map.empty (module IntPair))
      ~f:(fun map signal ->
        Map.set map ~key:(uid signal, 0) ~data:(List.hd_exn (names signal)))
  ;;

  let init_mangler_and_port_names ports reserved =
    (* initialise the mangler with all reserved words and module ports *)
    let mangler = Mangler.create ~case_sensitive:true in
    Mangler.add_identifiers_exn
      mangler
      (List.map ports ~f:(fun x -> List.hd_exn (names x)) @ reserved);
    (* initialize the name map with inputs and outputs *)
    let name_map = port_names_map ports in
    mangler, name_map
  ;;

  (* Mangle all names - creating a map of signal uid's to names.  First, add all inputs,
     outputs and reserved words - they must not be mangled or things get very confusing.
     Then the rest of the signals. *)
  let create reserved prefix circuit =
    let inputs = Circuit.inputs circuit in
    let outputs = Circuit.outputs circuit in
    let ios = inputs @ outputs in
    let ioset = Set.of_list (module Signal.Uid) (List.map ios ~f:uid) in
    (* initialise the mangler with all reserved words and the inputs+outputs *)
    let mangler, name_map = init_mangler_and_port_names ios reserved in
    (* add name to mangler and name map *)
    let generated_name map uid =
      let name = Mangler.mangle mangler (prefix ^ Int64.to_string uid) in
      Map.set map ~key:(uid, 0) ~data:name
    in
    let add_name map signal =
      let is_io signal = Set.mem ioset (uid signal) in
      if is_io signal || Signal.is_empty signal
      then (* IO signal names are handled seperately (they are not mangled) *)
        map
      else if List.is_empty (names signal)
      then (* generated name *)
        generated_name map (uid signal)
      else
        (* mangle signal names *)
        fst
          (List.fold (names signal) ~init:(map, 0) ~f:(fun (map, i) name ->
             let name = Mangler.mangle mangler name in
             Map.set map ~key:(uid signal, i) ~data:name, i + 1))
    in
    (* add special case for memories and instantiations *)
    let add_name map (signal : Signal.t) =
      match signal with
      | Mem { extra_uid; _ } -> add_name (generated_name map extra_uid) signal
      | Inst { extra_uid; _ } -> add_name (generated_name map extra_uid) signal
      | _ -> add_name map signal
    in
    let name_map =
      Signal_graph.depth_first_search
        (Signal_graph.create outputs)
        ~init:name_map
        ~f_before:add_name
    in
    let lookup name index = Map.find_exn name_map (name, index) in
    Staged.stage lookup
  ;;
end

(* internally traced nodes *)
module Internal_ports = struct
  let create circuit ~is_internal_port =
    (* create name mangler *)
    let name = Staged.unstage (Mangle_names.create [] "_" circuit) in
    let i =
      match is_internal_port with
      | None -> []
      | Some f ->
        Signal_graph.filter (Circuit.signal_graph circuit) ~f:(fun s ->
          (not (Circuit.is_input circuit s))
          && (not (Circuit.is_output circuit s))
          && (not (Signal.is_empty s))
          && f s)
    in
    (* Create a wire for each node, and give it the mangled names.  NOTE: these wire are
       required to make registers 'look' like they are updating correctly in simulation,
       however, they are not specifically needed for combinatorial nodes.  It does make the
       name mangling scheme a wee bit easier though. *)
    let dangler_uid = ref Int64.(shift_right_logical max_value 1) in
    let dangler s_width =
      let open Signal in
      Int64.incr dangler_uid;
      Wire
        { signal_id =
            { s_id = !dangler_uid
            ; s_names = []
            ; s_attributes = []
            ; s_width
            ; s_deps = []
            ; caller_id = None
            }
        ; driver = ref Empty
        }
    in
    List.map i ~f:(fun s ->
      let w = dangler (Signal.width s) in
      Signal.( <== ) w s;
      List.iteri (names s) ~f:(fun i _ ->
        ignore (Signal.( -- ) w (name (uid s) i) : Signal.t));
      w)
  ;;
end

module Maps : sig
  type t

  val create : Schedule.t -> t

  val find_data_by_uid_exn : t -> Signal.Uid.t -> Bits.Mutable.t
  val find_reg_by_uid_exn : t -> Signal.Uid.t -> Bits.Mutable.t
  val find_data_exn : t -> Signal.t -> Bits.Mutable.t
  val find_data : t -> Signal.t -> Bits.Mutable.t
  val find_reg_exn : t -> Signal.t -> Bits.Mutable.t
  val find_mem_exn : t -> Signal.t -> Bits.Mutable.t array
end = struct
  type t =
    { data : Bits.Mutable.t Map.M(Signal.Uid).t
    ; reg : Bits.Mutable.t Map.M(Signal.Uid).t
    ; mem : Bits.Mutable.t array Map.M(Signal.Uid).t
    ; schedule : Schedule.t
    }

  let uid = Signal.uid

  let alias_or_self (t : t) uid =
    match Schedule.resolve_alias t.schedule uid with
    | None -> uid
    | Some alias -> Signal.uid alias
  ;;

  let create (bundle : Schedule.t) =
    let zero n = Bits.Mutable.of_constant (Constant.of_int ~width:n 0) in
    let const c = Bits.Mutable.of_constant (Bits.to_constant c) in
    let data_map =
      let data_add map signal =
        if not (Schedule.is_alias bundle (uid signal))
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
        | Mem { memory = { mem_size; _ }; _ } | Multiport_mem { size = mem_size; _ } ->
          let mem = Array.init mem_size ~f:(fun _ -> zero (Signal.width signal)) in
          Map.set map ~key:(uid signal) ~data:mem
        | _ -> raise_s [%message "Expecting memory"]
      in
      List.fold (Schedule.mems bundle) ~init:(Map.empty (module Signal.Uid)) ~f:mem_add
    in
    { data = data_map; reg = reg_map; mem = mem_map; schedule = bundle }
  ;;

  let find_data_by_uid_exn maps uid = Map.find_exn maps.data (alias_or_self maps uid)

  let find_data_exn maps signal =
    find_data_by_uid_exn maps (alias_or_self maps (uid signal))
  ;;

  let find_data maps signal =
    match Map.find maps.data (alias_or_self maps (uid signal)) with
    | None -> Bits.Mutable.empty
    | Some bits -> bits
  ;;

  let find_reg_by_uid_exn maps uid = Map.find_exn maps.reg uid
  let find_reg_exn maps signal = find_reg_by_uid_exn maps (uid signal)
  let find_mem_exn maps signal = Map.find_exn maps.mem (uid signal)
end

module Io_ports = struct
  type 'a t =
    { in_ports : 'a
    ; out_ports : 'a
    ; internal_ports : 'a
    }

  let create circuit maps internal_ports : _ t =
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
    let internal_ports =
      List.concat
      @@ List.map internal_ports ~f:(fun signal ->
        List.map (names signal) ~f:(fun name -> name, Maps.find_data_exn maps signal))
    in
    { in_ports; out_ports; internal_ports }
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
  let zero n = Bits.Mutable.of_constant (Constant.of_int ~width:n 0)

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

  let signal ~schedule ~combinational_ops_database (maps : Maps.t) signal =
    let find_exn signal = Maps.find_data_exn maps signal in
    let find_or_empty signal = Maps.find_data maps signal in
    let tgt = find_exn signal in
    let deps = List.map (Signal.deps signal) ~f:find_or_empty in
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
           set0 tgt (int64_of_bool (Caml.Int64.unsigned_compare (get0 a) (get0 b) = -1))
         in
         let ltbig () = Bits.Mutable.( <: ) tgt a b in
         let width = Bits.Mutable.width a in
         if width <= 63 then Some ltsmall else if width = 64 then Some lt64 else Some ltbig)
    | Wire _ ->
      if not (Schedule.is_alias schedule (Signal.uid signal))
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
    | Mem { memory = m; _ } ->
      let mem = Maps.find_mem_exn maps signal in
      let addr = Maps.find_data_exn maps m.mem_read_address in
      let mem () =
        try Bits.Mutable.copy ~dst:tgt ~src:mem.(mutable_to_int addr) with
        | _ -> Bits.Mutable.copy ~dst:tgt ~src:(zero (Signal.width signal))
      in
      Some mem
    | Multiport_mem _ -> None
    | Mem_read_port { memory; read_address; _ } ->
      let mem = Maps.find_mem_exn maps memory in
      let addr = Maps.find_data_exn maps read_address in
      let zero = Bits.Mutable.create (Signal.width signal) in
      let mem_read () =
        let data =
          try mem.(mutable_to_int addr) with
          | _ -> zero
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
    | Mem { register = r; memory = m; _ } ->
      let mem = Maps.find_mem_exn maps signal in
      let we = Maps.find_data_exn maps r.reg_enable in
      let w = Maps.find_data_exn maps m.mem_write_address in
      let d = Maps.find_data_exn maps m.mem_write_data in
      let mem_upd () =
        if mutable_to_int we = 1
        then (
          let w = mutable_to_int w in
          if w >= m.mem_size
          then (*Printf.printf "memory write out of bounds %i/%i\n" w m.mem_size*)
            ()
          else Bits.Mutable.copy ~dst:mem.(w) ~src:d)
      in
      mem_upd
    | Multiport_mem { size; write_ports; _ } ->
      let mem = Maps.find_mem_exn maps signal in
      let multi_mem_wr (write_port : Signal.write_port) =
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
  let create ~schedule circuit tasks_comb =
    (* Find nodes between registers and outputs *)
    let nodes =
      Signal_graph.last_layer_of_nodes
        ~is_input:(Circuit.is_input circuit)
        (Circuit.signal_graph circuit)
      |> List.filter ~f:(fun uid -> not (Schedule.is_alias schedule uid))
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

module Compute_digest = struct
  let create out_ports_before out_ports_after internal_ports =
    let digest = ref (Md5_lib.string "digest of hardcaml simulation") in
    let digest_length = String.length (Md5_lib.to_binary !digest) in
    let ports =
      [ out_ports_before; out_ports_after; internal_ports ]
      |> List.concat
      |> List.map ~f:snd
    in
    let total_length =
      digest_length
      + List.fold ports ~init:0 ~f:(fun total bits ->
        total + Bits.number_of_data_bytes !bits)
    in
    let digestable_string = Bytes.init total_length ~f:(Fn.const '\000') in
    let build_digestable_string () =
      (* copy bits into digestable string *)
      let pos =
        List.fold ports ~init:digest_length ~f:(fun pos bits ->
          let length = Bits.number_of_data_bytes !bits in
          Bytes.blito
            ~src_pos:Bits.Expert.offset_for_data
            ~src:(Bits.Expert.unsafe_underlying_repr !bits)
            ~dst:digestable_string
            ~dst_pos:pos
            ();
          pos + length)
      in
      assert (pos = Bytes.length digestable_string)
    in
    let compute_digest () =
      build_digestable_string ();
      Bytes.blito
        ~src:(Md5_lib.to_binary !digest |> Bytes.of_string)
        ~dst:digestable_string
        ();
      digest := Md5_lib.bytes digestable_string
    in
    digest, compute_digest
  ;;
end

let create ?(config = Cyclesim0.Config.default) circuit =
  let circuit =
    if config.deduplicate_signals then Dedup.deduplicate circuit else circuit
  in
  (* add internally traced nodes *)
  let assertions = Circuit.assertions circuit in
  let internal_ports =
    Internal_ports.create circuit ~is_internal_port:config.is_internal_port
  in
  let bundle = Schedule.create circuit internal_ports in
  (* build maps *)
  let maps : Maps.t = Maps.create bundle in
  let compile =
    Compile.signal
      ~combinational_ops_database:config.combinational_ops_database
      ~schedule:bundle
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
  let { Io_ports.in_ports; out_ports; internal_ports } =
    Io_ports.create circuit maps internal_ports
  in
  let lookup_signal uid =
    ref (Maps.find_data_by_uid_exn maps uid |> Bits.Mutable.to_bits)
  in
  let lookup_reg uid = ref (Maps.find_reg_by_uid_exn maps uid |> Bits.Mutable.to_bits) in
  let violated_assertions = Hashtbl.create (module String) in
  let cycle_no = ref 0 in
  let check_assertions_task () =
    Map.iteri assertions ~f:(fun ~key ~data ->
      let asn_value = lookup_signal (Signal.uid data) in
      if Bits.is_gnd !asn_value
      then Hashtbl.add_multi violated_assertions ~key ~data:!cycle_no);
    cycle_no := !cycle_no + 1
  in
  let clear_violated_assertions_task () =
    Hashtbl.clear violated_assertions;
    cycle_no := 0
  in
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
  let internal_ports, internal_ports_task = out_ports_ref internal_ports in
  let tasks_before_clock_edge = List.map tasks_comb ~f:snd in
  let tasks_after_clock_edge =
    let optimize_last_layer = true in
    if optimize_last_layer
    then Last_layer.create ~schedule:bundle circuit tasks_comb
    else tasks_before_clock_edge
  in
  let digest, digest_task =
    Compute_digest.create
      out_ports_before_clock_edge
      out_ports_after_clock_edge
      internal_ports
  in
  { Cyclesim0.in_ports
  ; out_ports_after_clock_edge
  ; out_ports_before_clock_edge
  ; internal_ports
  ; inputs = in_ports
  ; outputs_after_clock_edge = out_ports_after_clock_edge
  ; outputs_before_clock_edge = out_ports_before_clock_edge
  ; cycle_check = task tasks_check
  ; cycle_before_clock_edge =
      tasks
        [ [ in_ports_task ]
        ; tasks_before_clock_edge
        ; tasks_regs
        ; [ out_ports_before_clock_edge_task; internal_ports_task ]
        ; [ check_assertions_task ]
        ]
  ; cycle_at_clock_edge = task tasks_at_clock_edge
  ; cycle_after_clock_edge =
      tasks
        [ tasks_after_clock_edge
        ; [ out_ports_task ]
        ; (if config.compute_digest then [ digest_task ] else [])
        ]
  ; reset = tasks [ resets; [ clear_violated_assertions_task ] ]
  ; lookup_signal
  ; lookup_reg
  ; assertions
  ; violated_assertions
  ; digest
  ; circuit = (if config.store_circuit then Some circuit else None)
  }
;;
