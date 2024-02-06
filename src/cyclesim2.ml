(* Optimisations are enabled for a few performance critical files in Hardcaml. This allows
   much better inlining and code gen, though we still lack cross module inlining. *)
[@@@ocaml.flambda_o3]

open Base
module Config = Cyclesim0.Config

module Node = struct
  type t =
    { signal : Signal.t
    ; address : int
    }
  [@@deriving sexp_of]
end

module Port = struct
  type t =
    { name : string
    ; address : int
    ; width : int
    }
end

(* Lists of different types of node with associated buffers and addresses. *)
module Nodes_and_addresses = struct
  type with_total_size =
    { nodes : Node.t list
    ; size : int
    }

  (* Wires with drivers are recorded with a reference to their (non-wire) driver. We later
     resolve this so we dont actually have to implement any logic for wires. The only
     wires which will exist are undriven ie inputs. *)
  type driven_wire =
    { wire : Signal.t
    ; driver : Signal.t
    }

  (* Constants are tracked and shared. *)
  module Constants = struct
    type t =
      { unique : Node.t Map.M(Bits).t
      ; map : Node.t Map.M(Signal.Uid).t
      }

    let empty = { unique = Map.empty (module Bits); map = Map.empty (module Signal.Uid) }

    let maybe_add t (node : Node.t) size =
      match node.signal with
      | Const { constant; _ } ->
        (match Map.find t.unique constant with
         | Some alias ->
           0, { t with map = Map.add_exn t.map ~key:(Signal.uid node.signal) ~data:alias }
         | None ->
           ( size
           , { unique = Map.add_exn t.unique ~key:constant ~data:node
             ; map = Map.add_exn t.map ~key:(Signal.uid node.signal) ~data:node
             } ))
      | _ -> raise_s [%message "[Constants.add] not constant" (node : Node.t)]
    ;;
  end

  type t =
    { comb : with_total_size
    ; regs : with_total_size
    ; regs_next : with_total_size
    ; mems : with_total_size
    ; consts : Constants.t
    ; wires : driven_wire list
    }

  let empty =
    { comb = { nodes = []; size = 0 }
    ; regs = { nodes = []; size = 0 }
    ; regs_next = { nodes = []; size = 0 }
    ; mems = { nodes = []; size = 0 }
    ; consts = Constants.empty
    ; wires = []
    }
  ;;

  let add_comb t comb size =
    { t with comb = { nodes = comb :: t.comb.nodes; size = t.comb.size + size } }
  ;;

  let add_reg t reg size =
    { t with regs = { nodes = reg :: t.regs.nodes; size = t.regs.size + size } }
  ;;

  let add_mem t mem size =
    { t with mems = { nodes = mem :: t.mems.nodes; size = t.mems.size + size } }
  ;;

  let add_const t const size =
    let size, consts = Constants.maybe_add t.consts const size in
    { t with comb = { nodes = t.comb.nodes; size = t.comb.size + size }; consts }
  ;;

  let add_wire t wire =
    let rec find_driver (signal : Signal.t) =
      match signal with
      | Wire { driver; _ } when not (Signal.is_empty !driver) -> find_driver !driver
      | _ -> signal
    in
    { t with wires = { wire; driver = find_driver wire } :: t.wires }
  ;;

  (* We build up the allocation lists in reverse, so put them back into the right order.

     In addition, we intend to pack all this data into a single array, so perform the
     address offsets here.

     The overall data layout will be;

     {v
          +-----------+
          | comb      |
          | ...       |
          +-----------+
          | regs      |
          | ...       |
          +-----------+
          | regs_next |
          | ...       |
          +-----------+
          | mems      |
          | ...       |
          +-----------+
     v}
  *)
  let reverse_and_flatten t =
    let regs_offset = t.comb.size in
    let regs_next_offset = t.comb.size + t.regs.size in
    let mems_offset =
      (* Note we use [t.regs.size] here as regs_next is a virtual space at this point - it
         becomes a copy of regs at a new offset below. *)
      regs_next_offset + t.regs.size
    in
    let offset by ({ signal; address } : Node.t) =
      { Node.signal; address = address + by }
    in
    { comb = { t.comb with nodes = List.rev t.comb.nodes }
    ; regs = { t.regs with nodes = List.rev_map t.regs.nodes ~f:(offset regs_offset) }
    ; regs_next =
        { t.regs with nodes = List.rev_map t.regs.nodes ~f:(offset regs_next_offset) }
    ; mems = { t.mems with nodes = List.rev_map t.mems.nodes ~f:(offset mems_offset) }
    ; consts = t.consts
    ; wires = t.wires
    }
  ;;
end

(* Mapping from signals to nodes. Note we don't touch regs_next here as it used during the
   sequential update phase. *)
module Read_map = struct
  type t = Node.t Map.M(Signal.Uid).t

  let create (t : Nodes_and_addresses.t) : t =
    let map = t.consts.map in
    let map =
      List.fold t.comb.nodes ~init:map ~f:(fun map comb ->
        Map.add_exn map ~key:(Signal.uid comb.signal) ~data:comb)
    in
    let map =
      List.fold t.regs.nodes ~init:map ~f:(fun map reg ->
        Map.add_exn map ~key:(Signal.uid reg.signal) ~data:reg)
    in
    let map =
      List.fold t.mems.nodes ~init:map ~f:(fun map mem ->
        Map.add_exn map ~key:(Signal.uid mem.signal) ~data:mem)
    in
    (* add wire references to (existing) driver *)
    let map =
      List.fold t.wires ~init:map ~f:(fun map { wire; driver } ->
        let driver = Map.find_exn map (Signal.uid driver) in
        Map.add_exn map ~key:(Signal.uid wire) ~data:driver)
    in
    map
  ;;
end

let num_words s = (Signal.width s + 63) / 64
let names s = if Signal.is_empty s then [] else Signal.names s

(* Create nodes with addresses for each signal in the circuit. *)
let allocate_addresses (schedule : Signal.t list) =
  List.fold schedule ~init:Nodes_and_addresses.empty ~f:(fun acc signal ->
    match signal with
    | Empty -> acc
    | Const _ ->
      Nodes_and_addresses.add_const
        acc
        { signal; address = acc.comb.size }
        (num_words signal)
    | Reg _ ->
      Nodes_and_addresses.add_reg
        acc
        { signal; address = acc.regs.size }
        (num_words signal)
    | Multiport_mem mem ->
      let num_words = num_words signal in
      let mem_size_in_words = num_words * mem.size in
      Nodes_and_addresses.add_mem
        acc
        { signal; address = acc.mems.size }
        mem_size_in_words
    | Wire { driver; _ } when not (Signal.is_empty !driver) ->
      Nodes_and_addresses.add_wire acc signal
    | _ ->
      Nodes_and_addresses.add_comb
        acc
        { signal; address = acc.comb.size }
        (num_words signal))
  |> Nodes_and_addresses.reverse_and_flatten
;;

(* Create a static array of addresses and values of constants. *)
let init_consts (consts : Nodes_and_addresses.Constants.t) =
  List.map (Map.to_alist consts.unique) ~f:(fun (_, t) ->
    match t.signal with
    | Const { constant; _ } ->
      let size_in_words = num_words t.signal in
      let constant = Array.init size_in_words ~f:(Bits.unsafe_get_int64 constant) in
      t.address, constant
    | _ -> raise_s [%message "[init_consts] unexpected signal" (t : Node.t)])
  |> Array.of_list
;;

let reset_consts (regs : Node.t list) =
  List.filter_map regs ~f:(fun reg ->
    match reg.signal with
    | Reg { register; _ } ->
      if not (Signal.is_empty register.reg_reset)
      then (
        let reset = register.reg_reset_value in
        if Signal.Type.is_const reset
        then (
          let constant = Signal.to_constant reset |> Bits.of_constant in
          let size_in_words = num_words reg.signal in
          let reset = Array.init size_in_words ~f:(Bits.unsafe_get_int64 constant) in
          Some (reg.address, reset))
        else raise_s [%message "Reset values must be constants" (reg : Node.t)])
      else None
    | _ -> raise_s [%message "[compile_reg] expecting reg"])
  |> Array.of_list
;;

let memory_size (memory : Signal.t) =
  match memory with
  | Multiport_mem { size; _ } -> size
  | _ -> raise_s [%message "Mem_read_port must reference Multiport_mem"]
;;

(* Generate code for each combinational signal in the circuit. Excludes registers,
   constants and memories. *)
let compile_comb
  t
  ~combinational_ops_database
  (map : Read_map.t)
  ({ address = dst_address; signal = dst_signal } : Node.t)
  =
  let find_exn signal = Map.find_exn map (Signal.uid signal) in
  let find_address signal = (find_exn signal).address in
  let dst_width = Signal.width dst_signal in
  match dst_signal with
  | Op2 { arg_a; arg_b; op = Signal_add; _ } ->
    let src_address_a = find_address arg_a in
    let src_address_b = find_address arg_b in
    let width_in_bits = dst_width in
    Some (Cyclesim2_ops.add t ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Signal_sub; _ } ->
    let src_address_a = find_address arg_a in
    let src_address_b = find_address arg_b in
    let width_in_bits = dst_width in
    Some (Cyclesim2_ops.sub t ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Signal_mulu; _ } ->
    let arg_a = Map.find_exn map (Signal.uid arg_a) in
    let arg_b = Map.find_exn map (Signal.uid arg_b) in
    let src_address_a, width_in_bits_a = arg_a.address, Signal.width arg_a.signal in
    let src_address_b, width_in_bits_b = arg_b.address, Signal.width arg_b.signal in
    Some
      (Cyclesim2_ops.mulu
         t
         ~dst_address
         ~src_address_a
         ~src_address_b
         ~width_in_bits_a
         ~width_in_bits_b)
  | Op2 { arg_a; arg_b; op = Signal_muls; _ } ->
    let arg_a = Map.find_exn map (Signal.uid arg_a) in
    let arg_b = Map.find_exn map (Signal.uid arg_b) in
    let src_address_a, width_in_bits_a = arg_a.address, Signal.width arg_a.signal in
    let src_address_b, width_in_bits_b = arg_b.address, Signal.width arg_b.signal in
    Some
      (Cyclesim2_ops.muls
         t
         ~dst_address
         ~src_address_a
         ~src_address_b
         ~width_in_bits_a
         ~width_in_bits_b)
  | Op2 { arg_a; arg_b; op = Signal_and; _ } ->
    let src_address_a = find_address arg_a in
    let src_address_b = find_address arg_b in
    let width_in_bits = dst_width in
    Some (Cyclesim2_ops.and_ t ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Signal_or; _ } ->
    let src_address_a = find_address arg_a in
    let src_address_b = find_address arg_b in
    let width_in_bits = dst_width in
    Some (Cyclesim2_ops.or_ t ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Signal_xor; _ } ->
    let src_address_a = find_address arg_a in
    let src_address_b = find_address arg_b in
    let width_in_bits = dst_width in
    Some (Cyclesim2_ops.xor t ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Signal_eq; _ } ->
    let { Node.address = src_address_a; signal = arg_a } = find_exn arg_a in
    let src_address_b = find_address arg_b in
    let width_in_bits = Signal.width arg_a in
    Some (Cyclesim2_ops.eq t ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Signal_lt; _ } ->
    let { Node.address = src_address_a; signal = arg_a } = find_exn arg_a in
    let src_address_b = find_address arg_b in
    let width_in_bits = Signal.width arg_a in
    Some (Cyclesim2_ops.lt t ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Not { arg; _ } ->
    let src_address = find_address arg in
    let width_in_bits = dst_width in
    Some (Cyclesim2_ops.not_ t ~dst_address ~src_address ~width_in_bits)
  | Mux { select; cases; _ } ->
    let select_address = find_address select in
    let cases = List.map cases ~f:find_address |> Array.of_list in
    let size_in_words = num_words dst_signal in
    let select_width = Signal.width select in
    Some
      (Cyclesim2_ops.mux
         t
         ~dst_address
         ~select_address
         ~select_width
         ~cases
         ~size_in_words)
  | Cat { args; _ } ->
    let cat_src =
      List.map args ~f:(fun arg ->
        let { Node.address; signal } = find_exn arg in
        { Bits_packed.Cat_src.address; width = Signal.width signal })
    in
    let width_in_bits = dst_width in
    Some (Cyclesim2_ops.cat t ~dst_address cat_src ~width_in_bits)
  | Select { arg; high; low; _ } ->
    let { Node.address = src_address; signal = _ } = find_exn arg in
    Some (Cyclesim2_ops.select t ~dst_address ~src_address ~high ~low)
  | Wire { driver; _ } ->
    if not (Signal.is_empty !driver)
    then
      raise_s
        [%message
          "[compile_comb] driven wires should be removed from the simulator model"];
    None
  | Mem_read_port { memory; read_address; _ } ->
    let size_in_words = num_words dst_signal in
    let read_address = find_address read_address in
    let memory_address = find_address memory in
    let memory_size = memory_size memory in
    Some
      (Cyclesim2_ops.mem_read
         t
         ~dst_address
         ~read_address
         ~memory_address
         ~memory_size
         ~size_in_words)
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
       let inputs =
         List.map i.inst_inputs ~f:(fun (_, signal) ->
           Bits.Mutable.create (Signal.width signal))
       in
       let copy_inputs =
         List.map2_exn i.inst_inputs inputs ~f:(fun (_, signal) bits ->
           let src_address = find_address signal in
           let size_in_words = num_words signal in
           fun () ->
             for i = 0 to size_in_words - 1 do
               Bits.Mutable.set_int64 bits i (Cyclesim2_ops.get64 t (src_address + i))
             done)
       in
       let outputs =
         List.map i.inst_outputs ~f:(fun (_, (bits, _)) -> Bits.Mutable.create bits)
       in
       let target = Bits.Mutable.create dst_width in
       let copy_output =
         let size_in_words = num_words dst_signal in
         fun () ->
           for i = 0 to size_in_words - 1 do
             Cyclesim2_ops.set64 t (dst_address + i) (Bits.Mutable.get_int64 target i)
           done
       in
       let inst () =
         List.iter copy_inputs ~f:(fun f -> f ());
         f inputs outputs;
         Bits.Mutable.concat target (List.rev outputs);
         copy_output ()
       in
       Some inst)
  (* Unsupported here *)
  | Empty -> raise_s [%message "[compile_comb] empty signal" (dst_signal : Signal.t)]
  | Const _ -> raise_s [%message "[compile_comb] constant" (dst_signal : Signal.t)]
  | Reg _ -> raise_s [%message "[compile_comb] reg" (dst_signal : Signal.t)]
  | Multiport_mem _ ->
    raise_s [%message "[compile_comb] multiport mem" (dst_signal : Signal.t)]
;;

(* Generate code for updating registers. *)
let compile_reg_update t (map : Read_map.t) (dst : Node.t) =
  let find_exn s = Map.find_exn map (Signal.uid s) in
  let find_address s = (find_exn s).address in
  let size_in_words = num_words dst.signal in
  match dst.signal with
  | Reg { register; d; _ } ->
    let dst_address = dst.address in
    let src_address = find_address d in
    let clear =
      if not (Signal.is_empty register.reg_clear)
      then
        Some
          { Cyclesim2_ops.clear = find_address register.reg_clear
          ; clear_value = find_address register.reg_clear_value
          ; level =
              (match register.reg_clear_level with
               | High -> 1
               | Low -> 0)
          }
      else None
    in
    let enable =
      if not (Signal.is_empty register.reg_enable)
      then Some (find_address register.reg_enable)
      else None
    in
    Cyclesim2_ops.reg t ~clear ~enable ~dst_address ~src_address ~size_in_words
  | _ -> raise_s [%message "[compile_reg_update] expecting reg"]
;;

let compile_mem_update t (map : Read_map.t) (dst : Node.t) =
  let find_address s = (Map.find_exn map (Signal.uid s)).address in
  match dst.signal with
  | Multiport_mem { size; write_ports; _ } ->
    let size_in_words = num_words dst.signal in
    let memory_address = find_address dst.signal in
    let port_updates =
      Array.map
        write_ports
        ~f:(fun { write_clock = _; write_enable; write_address; write_data } ->
        let write_enable = find_address write_enable in
        let write_address = find_address write_address in
        let write_data = find_address write_data in
        Cyclesim2_ops.mem_write_port
          t
          ~size
          ~memory_address
          ~write_enable
          ~write_address
          ~write_data
          ~size_in_words)
    in
    fun () -> Array.iter port_updates ~f:(fun f -> f ())
  | _ -> raise_s [%message "[compile_mem_update] expecting memory"]
;;

let last_layer circuit (allocation : Node.t list) =
  let nodes =
    Signal_graph.last_layer_of_nodes
      ~is_input:(Circuit.is_input circuit)
      (Circuit.signal_graph circuit)
    |> Set.of_list (module Signal.Uid)
  in
  List.filter allocation ~f:(fun alloc -> Set.mem nodes (Signal.uid alloc.signal))
;;

type t =
  { init_consts : (int * Int64.t array) array
  ; reset_consts : (int * Int64.t array) array
  ; comb : unit -> unit
  ; comb_last_layer : unit -> unit
  ; reg_update : unit -> unit
  ; mem_update : unit -> unit
  ; in_ports : Port.t list
  ; out_ports : Port.t list
  ; runtime : Bytes.t
  }

type offset =
  { address : int
  ; size : int
  }
[@@deriving sexp_of]

type offsets =
  { comb : offset
  ; regs : offset
  ; regs_next : offset
  ; mems : offset
  }
[@@deriving sexp_of]

let copy_in_ports (t : Bytes.t) (ports : (Port.t * Bits.t ref) list) =
  List.iter ports ~f:(fun ({ name = _; address; width = _ }, bits) ->
    let bytes = Bits.Expert.unsafe_underlying_repr !bits in
    let header_size = Bits.Expert.offset_for_data in
    Bytes.unsafe_blit
      ~src:bytes
      ~src_pos:header_size
      ~dst:t
      ~dst_pos:(address * 8)
      ~len:(Bytes.length bytes - header_size))
;;

let copy_out_ports (t : Bytes.t) (ports : (Port.t * Bits.t ref) list) =
  List.iter ports ~f:(fun ({ name = _; address; width }, bits) ->
    let new_bits = Bits.zero width in
    let bytes = Bits.Expert.unsafe_underlying_repr new_bits in
    let header_size = Bits.Expert.offset_for_data in
    Bytes.unsafe_blit
      ~src:t
      ~src_pos:(address * 8)
      ~dst:bytes
      ~dst_pos:header_size
      ~len:(Bytes.length bytes - header_size);
    bits := new_bits)
;;

let lookup_node runtime (map : Read_map.t) (traced : Cyclesim0.Traced.internal_signal) =
  if not (Signal.Type.is_mem traced.signal || Signal.Type.is_reg traced.signal)
  then
    Map.find map (Signal.uid traced.signal)
    |> Option.map ~f:(fun { address; signal } ->
         Cyclesim0.Node.create_from_signal
           ~byte_address:(address * 8)
           ~data:runtime
           signal)
  else None
;;

let lookup_reg runtime (map : Read_map.t) (traced : Cyclesim0.Traced.internal_signal) =
  if Signal.Type.is_reg traced.signal
  then
    Map.find map (Signal.uid traced.signal)
    |> Option.map ~f:(fun { address; signal } ->
         Cyclesim0.Reg.create_from_signal ~byte_address:(address * 8) ~data:runtime signal)
  else None
;;

let lookup_mem runtime (map : Read_map.t) (traced : Cyclesim0.Traced.internal_signal) =
  if Signal.Type.is_mem traced.signal
  then
    Map.find map (Signal.uid traced.signal)
    |> Option.map ~f:(fun { address; _ } ->
         Cyclesim0.Memory.create_from_signal
           ~byte_address:(address * 8)
           ~data:runtime
           traced.signal)
  else None
;;

let create_cyclesim
  (offsets : offsets)
  (map : Read_map.t)
  (traced : Cyclesim0.Traced.t)
  { init_consts
  ; reset_consts
  ; comb
  ; comb_last_layer
  ; reg_update
  ; mem_update
  ; in_ports
  ; out_ports
  ; runtime
  }
  =
  (* Initialize constant signals *)
  let set_consts consts =
    Array.iter consts ~f:(fun (address, data) ->
      Array.iteri data ~f:(fun i data ->
        Bytes.unsafe_set_int64 runtime ((address + i) * 8) data))
  in
  set_consts init_consts;
  let reset () = set_consts reset_consts in
  (* Simulation I/O ports *)
  let ports =
    List.map ~f:(fun ({ Port.name = _; address = _; width } as port) ->
      port, ref (Bits.zero width))
  in
  let in_ports = ports in_ports in
  let out_ports_before = ports out_ports in
  let out_ports_after = ports out_ports in
  (* Simulation steps *)
  let cycle_check () =
    List.iter in_ports ~f:(fun ({ Port.name; address = _; width }, bits) ->
      if Bits.width !bits <> width
      then
        raise_s
          [%message
            "Invalid input port width" (name : string) (width : int) (bits : Bits.t ref)])
  in
  let cycle_before_clock_edge () =
    (* copy in inputs *)
    copy_in_ports runtime in_ports;
    (* main combinational step *)
    comb ();
    (* grab outputs before clock edge *)
    copy_out_ports runtime out_ports_before;
    (* perform register updates *)
    reg_update ()
  in
  let cycle_at_clock_edge () =
    (* run memory writes *)
    mem_update ();
    (* copy back new register values *)
    Bytes.blito
      ~dst:runtime
      ~dst_pos:offsets.regs.address
      ~src:runtime
      ~src_pos:offsets.regs_next.address
      ~src_len:offsets.regs.size
      ()
  in
  let cycle_after_clock_edge () =
    (* update combinational outputs wrt to new register values *)
    comb_last_layer ();
    (* copy output ports *)
    copy_out_ports runtime out_ports_after
  in
  let get_ports (ports : (Port.t * Bits.t ref) list) =
    List.map ports ~f:(fun ({ name; address = _; width = _ }, bits) -> name, bits)
  in
  Cyclesim0.Private.create
    ~in_ports:(get_ports in_ports)
    ~out_ports_before_clock_edge:(get_ports out_ports_before)
    ~out_ports_after_clock_edge:(get_ports out_ports_after)
    ~reset
    ~cycle_check
    ~cycle_before_clock_edge
    ~cycle_at_clock_edge
    ~cycle_after_clock_edge
    ~traced
    ~lookup_node:(lookup_node runtime map)
    ~lookup_reg:(lookup_reg runtime map)
    ~lookup_mem:(lookup_mem runtime map)
    ()
;;

let port (map : Read_map.t) signal =
  let find_exn s = Map.find_exn map (Signal.uid s) in
  let find_address s = (find_exn s).address in
  let name = names signal |> List.hd_exn in
  { Port.address = find_address signal; name; width = Signal.width signal }
;;

let create ?(config = Cyclesim0.Config.default) circuit =
  let circuit =
    if config.deduplicate_signals then Dedup.deduplicate circuit else circuit
  in
  let traced =
    Cyclesim_compile.Traced_nodes.create circuit ~is_internal_port:config.is_internal_port
  in
  let outputs = Circuit.outputs circuit in
  let signal_graph = Signal_graph.create outputs in
  let schedule =
    if List.is_empty outputs
    then []
    else
      Signal_graph.topological_sort_exn
        ~deps:(module Signal_graph.Deps_for_simulation_scheduling)
        signal_graph
  in
  let allocation = allocate_addresses schedule in
  let map = Read_map.create allocation in
  let init_consts = init_consts allocation.consts in
  let reset_consts =
    (* We must reset both the current and next values - if the register is not enabled in
       the subsequent cycle it wont change the 'reg_next' value - and the reset gets lost.
    *)
    Array.concat
      [ reset_consts allocation.regs.nodes; reset_consts allocation.regs_next.nodes ]
  in
  let offsets, size =
    let add prev size =
      let size = size * 8 in
      { address = prev.address + prev.size; size }
    in
    let comb = add { address = 0; size = 0 } allocation.comb.size in
    let regs = add comb allocation.regs.size in
    let regs_next = add regs allocation.regs_next.size in
    let mems = add regs_next allocation.mems.size in
    { comb; regs; regs_next; mems }, mems.address + mems.size
  in
  let runtime = Bytes.make size '\000' in
  let comb =
    List.filter_map
      allocation.comb.nodes
      ~f:
        (compile_comb
           ~combinational_ops_database:config.combinational_ops_database
           runtime
           map)
  in
  let comb_last_layer =
    List.filter_map
      (last_layer circuit allocation.comb.nodes)
      ~f:
        (compile_comb
           ~combinational_ops_database:config.combinational_ops_database
           runtime
           map)
  in
  let reg_update =
    List.map allocation.regs_next.nodes ~f:(compile_reg_update runtime map)
  in
  let mem_update = List.map allocation.mems.nodes ~f:(compile_mem_update runtime map) in
  let run tasks =
    let tasks = Array.of_list tasks in
    let len = Array.length tasks in
    fun () ->
      for i = 0 to len - 1 do
        (Array.unsafe_get tasks i) ()
      done
  in
  let comb = run comb in
  let comb_last_layer = run comb_last_layer in
  let reg_update = run reg_update in
  let mem_update = run mem_update in
  create_cyclesim
    offsets
    map
    traced
    { init_consts
    ; reset_consts
    ; comb
    ; comb_last_layer
    ; reg_update
    ; mem_update
    ; in_ports = List.map (Circuit.inputs circuit) ~f:(port map)
    ; out_ports = List.map (Circuit.outputs circuit) ~f:(port map)
    ; runtime
    }
;;
