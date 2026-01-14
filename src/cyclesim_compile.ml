(* Optimisations are enabled for a few performance critical files in Hardcaml. This allows
   much better inlining and code gen, though we still lack cross module inlining. *)
[@@@ocaml.flambda_o3]

open! Core0
module Config = Cyclesim0.Config
module Clock_domain = Cyclesim_clock_domain

module Node = struct
  type t =
    { signal : Signal.t
    ; word_address : int
    }
  [@@deriving sexp_of]
end

module Port = struct
  type t =
    { name : string
    ; address : int
    ; width : int
    ; bits : Bits.t ref
    }
  [@@deriving sexp_of]
end

let num_words s = (Signal.width s + 63) / 64
let names s = if Signal.is_empty s then [] else Signal.names s
let port_name s = names s |> List.hd_exn

module Clocks = struct
  module Cycle = struct
    type t =
      | Not_started
      | Cycle of int
    [@@deriving sexp_of]
  end

  module Multi_domain = struct
    type t =
      { domains : Clock_domain.Group.t
      ; clocks_to_domains : (Clock_domain.indexed * Signal.t) Map.M(Signal.Type.Uid).t
      ; double_frequency : bool
          (* True when we need to step two cycles per user cycle. This is necessary when
             there are clocks with a odd period so that we can have them fall half way
             through the period. *)
      ; mutable cycle : Cycle.t
          (* Clock periods and edges work as follows: the [before_edge] triggers at the
             start of the period and the [at_edge] at the very end of the period.

             For example:
             - clock with period = 1: 0 (before and at), 1 (before and at),..
             - clock with period = 2: 0 (before), 1 (at), 2 (before), 3 (at), ..
             - clock with period = 3: 0 (before), 1, 2 (at), 3 (before), ...

             Inputs are updated at the [before_edge] and memory/registers are updated at
             the [at_edge].
          *)
      ; clocks_before_edge_this_cycle : Clock_domain.Set.t
      ; clocks_at_edge_this_cycle : Clock_domain.Set.t
      }
    [@@deriving sexp_of]

    let next_cycle t : Cycle.t =
      match t.cycle with
      | Cycle cycle -> Cycle (cycle + 1)
      | Not_started -> Cycle 0
    ;;

    let mark_cycle t cycle =
      t.cycle <- cycle;
      match cycle with
      | Not_started ->
        Clock_domain.Set.clear t.clocks_before_edge_this_cycle;
        Clock_domain.Set.clear t.clocks_at_edge_this_cycle
      | Cycle cycle ->
        Clock_domain.Group.elements t.domains
        |> Iarray.iter ~f:(fun indexed ->
          let set set ~aligned_to =
            if Clock_domain.aligned (Clock_domain.domain indexed) ~cycle:aligned_to
            then Clock_domain.Set.add set indexed
            else Clock_domain.Set.remove set indexed
          in
          set t.clocks_before_edge_this_cycle ~aligned_to:cycle;
          set t.clocks_at_edge_this_cycle ~aligned_to:(cycle + 1))
    ;;

    let reset t = mark_cycle t Not_started
    let incr_cycle t = mark_cycle t (next_cycle t)

    let create circuit (clocks : Clock_domain.t list) =
      let double_frequency, clocks =
        if List.exists clocks ~f:(fun { period; _ } -> period % 2 <> 0)
        then
          true, List.map clocks ~f:(fun clock -> { clock with period = clock.period * 2 })
        else false, clocks
      in
      let domains =
        match Clock_domain.Group.create clocks with
        | `Ok group -> group
        | `Duplicate_key name ->
          raise_s
            [%message
              "Multiple clock domains with the same name" (name : Clock_domain.Name.t)]
      in
      let resolved_clock_signals =
        Signal_graph.resolve_clock_domains (Circuit.signal_graph circuit)
      in
      let clocks_to_domains =
        Map.map resolved_clock_signals ~f:(fun signal ->
          let name = port_name signal |> Clock_domain.Name.of_string in
          match Clock_domain.Group.get domains name with
          | Some indexed -> indexed, signal
          | None ->
            raise_s
              [%message
                "No clock domain given for clock signal" (name : Clock_domain.Name.t)])
      in
      let t =
        { domains
        ; clocks_to_domains
        ; double_frequency
        ; cycle = Not_started
        ; clocks_before_edge_this_cycle = Clock_domain.Set.create domains ~default:false
        ; clocks_at_edge_this_cycle = Clock_domain.Set.create domains ~default:false
        }
      in
      reset t;
      t
    ;;

    let find_clock_exn t clock_signal =
      Map.find_exn t.clocks_to_domains (Signal.Type.uid clock_signal) |> fst
    ;;

    let aligned t =
      match next_cycle t with
      | Not_started -> false
      | Cycle cycle ->
        Clock_domain.Group.elements t.domains
        |> Iarray.for_all ~f:(fun indexed ->
          Clock_domain.aligned (Clock_domain.domain indexed) ~cycle)
    ;;

    let clock_multiple t = if t.double_frequency then 2 else 1

    let is_true_cycle t side =
      match t.cycle with
      | Not_started -> false
      | Cycle cycle ->
        (match t.double_frequency with
         | false -> true
         | true ->
           (match side with
            | `Before_edge -> cycle % 2 = 0
            | `At_edge -> (cycle + 1) % 2 = 0))
    ;;
  end

  type t =
    | Single_domain of
        { indexed : Clock_domain.indexed
        ; group : Clock_domain.Group.t
        ; set : Clock_domain.Set.t
        }
    | Multi_domain of Multi_domain.t
  [@@deriving sexp_of]

  let create circuit (clock_mode : Config.Clock_mode.t) =
    match clock_mode with
    | All_one_domain ->
      let domain =
        { Clock_domain.name = Clock_domain.Name.of_string "clock"; period = 1 }
      in
      let group = Clock_domain.Group.create_exn [ domain ] in
      let indexed = Clock_domain.Group.get group domain.name |> Option.value_exn in
      let set = Clock_domain.Set.create group ~default:true in
      Single_domain { indexed; group; set }
    | By_input_clocks clocks -> Multi_domain (Multi_domain.create circuit clocks)
  ;;

  let domains = function
    | Single_domain { group; _ } -> group
    | Multi_domain { domains; _ } -> domains
  ;;

  let cycle_multiple = function
    | Single_domain _ -> 1
    | Multi_domain multi -> Multi_domain.clock_multiple multi
  ;;

  let is_true_cycle t side =
    match t with
    | Single_domain _ -> true
    | Multi_domain multi -> Multi_domain.is_true_cycle multi side
  ;;

  let find_clock_exn t signal =
    match t with
    | Single_domain { indexed; _ } -> indexed
    | Multi_domain multi -> Multi_domain.find_clock_exn multi signal
  ;;

  let clocks_this_cycle t side =
    match t with
    | Single_domain { set; _ } -> set
    | Multi_domain { clocks_before_edge_this_cycle; clocks_at_edge_this_cycle; _ } ->
      (match side with
       | `Before_edge -> clocks_before_edge_this_cycle
       | `At_edge -> clocks_at_edge_this_cycle)
  ;;

  let reset = function
    | Single_domain _ -> ()
    | Multi_domain multi -> Multi_domain.reset multi
  ;;

  let incr_cycle = function
    | Single_domain _ -> ()
    | Multi_domain multi -> Multi_domain.incr_cycle multi
  ;;

  let aligned = function
    | Single_domain _ -> true
    | Multi_domain multi -> Multi_domain.aligned multi
  ;;
end

(* Lists of different types of node with associated buffers and addresses. *)
module Nodes_and_addresses = struct
  module Builder = struct
    type node =
      { signal : Signal.t
      ; size : int
      }

    (* Wires with drivers are recorded with a reference to their (non-wire) driver. We
       later resolve this so we dont actually have to implement any logic for wires. The
       only wires which will exist are undriven ie inputs. *)
    type driven_wire =
      { wire : Signal.t
      ; driver : Signal.t
      }

    module Constant = struct
      type t =
        { node : node
        ; references : Signal.Type.Uid_set.t
        (* Additional signals that reference the same bits *)
        }

      let create signal size =
        { node = { signal; size }; references = Set.empty (module Signal.Type.Uid) }
      ;;

      let add t signal = { t with references = Set.add t.references (Signal.uid signal) }
    end

    type t =
      { clocks : Clocks.t
      ; comb : node list
      ; regs : node list Clock_domain.Table.t
      ; mems : node list
      ; consts : Constant.t Map.M(Bits).t
      ; wires : driven_wire list
      }

    let empty clocks =
      { clocks
      ; comb = []
      ; regs = Clock_domain.Table.create (Clocks.domains clocks) []
      ; mems = []
      ; consts = Map.empty (module Bits)
      ; wires = []
      }
    ;;

    let add_to_section section signal size =
      (* Constructing the nodes list this was reverses the order they are added in.
         However the build function reverses them back. *)
      { signal; size } :: section
    ;;

    let add_comb t comb size = { t with comb = add_to_section t.comb comb size }

    let add_reg t reg clock size =
      let section = Clock_domain.Table.get t.regs clock in
      Clock_domain.Table.set t.regs ~key:clock ~data:(add_to_section section reg size);
      t
    ;;

    let add_mem t mem size = { t with mems = add_to_section t.mems mem size }

    let add_const t (signal : Signal.t) size =
      match signal with
      | Const { constant; _ } ->
        let consts =
          Map.update t.consts constant ~f:(fun existing ->
            match existing with
            | None -> Constant.create signal size
            | Some const -> Constant.add const signal)
        in
        { t with consts }
      | _ -> raise_s [%message "[add_const] not constant" (signal : Signal.t)]
    ;;

    let add_wire t wire =
      let rec find_driver (signal : Signal.t) =
        match signal with
        | Wire { driver = Some driver; _ } -> find_driver driver
        | _ -> signal
      in
      { t with wires = { wire; driver = find_driver wire } :: t.wires }
    ;;
  end

  type section =
    { nodes : Node.t list
    ; byte_address : int
    ; size_bytes : int
    }

  type t =
    { comb : section
    ; regs : section Clock_domain.Table.t
    ; regs_next : section Clock_domain.Table.t
    ; mems : section
    ; consts : section
    ; regs_next_nodes : Node.t list
    ; regs_nodes : Node.t list
    ; map : Node.t Map.M(Signal.Type.Uid).t
    }

  let create_section nodes ~offset ~new_offset =
    { nodes; byte_address = offset * 8; size_bytes = (new_offset - offset) * 8 }
  ;;

  let allocate_section ?(add_to_map = true) map ls ~node_fn ~references_fn ~offset =
    let map, nodes, new_offset =
      List.fold ls ~init:(map, [], offset) ~f:(fun (map, nodes, offset) a ->
        let%tydi { Builder.signal; size } = node_fn a in
        let ids = Signal.uid signal :: references_fn a in
        let node = { Node.signal; word_address = offset } in
        let map =
          match add_to_map with
          | false -> map
          | true ->
            List.fold ids ~init:map ~f:(fun map id -> Map.add_exn map ~key:id ~data:node)
        in
        (* Rebuilding the nodes list this way reverses the ordering, preserving the
           original order they were added to the builder. *)
        map, node :: nodes, offset + size)
    in
    map, create_section nodes ~offset ~new_offset, new_offset
  ;;

  let allocate_nodes ?add_to_map map nodes ~offset =
    allocate_section
      ?add_to_map
      map
      nodes
      ~node_fn:Fn.id
      ~references_fn:(Fn.const [])
      ~offset
  ;;

  let allocate_regs ?add_to_map map regs ~offset =
    let (map, offset), regs =
      Clock_domain.Table.fold_map regs ~init:(map, offset) ~f:(fun (map, offset) regs ->
        let map, regs, offset = allocate_nodes ?add_to_map map regs ~offset in
        (map, offset), regs)
    in
    map, regs, offset
  ;;

  let allocate_consts map consts ~offset =
    allocate_section
      map
      (Map.data consts)
      ~node_fn:(fun { Builder.Constant.node; _ } -> node)
      ~references_fn:(fun { Builder.Constant.references; _ } -> Set.to_list references)
      ~offset
  ;;

  let build (builder : Builder.t) =
    let map = Map.empty (module Signal.Type.Uid) in
    let map, comb, offset = allocate_nodes map builder.comb ~offset:0 in
    let map, regs, offset = allocate_regs map builder.regs ~offset in
    let map, regs_next, offset =
      allocate_regs map builder.regs ~add_to_map:false ~offset
    in
    let map, mems, offset = allocate_nodes map builder.mems ~offset in
    let map, consts, _offset = allocate_consts map builder.consts ~offset in
    let map =
      List.fold builder.wires ~init:map ~f:(fun map { wire; driver } ->
        let driver = Map.find_exn map (Signal.uid driver) in
        Map.add_exn map ~key:(Signal.uid wire) ~data:driver)
    in
    let compute_reg_nodes reg_section =
      Clock_domain.Group.elements (Clocks.domains builder.clocks)
      |> Iarray.to_list
      |> List.concat_map ~f:(fun clock ->
        let { nodes; _ } = Clock_domain.Table.get reg_section clock in
        nodes)
    in
    { comb
    ; regs
    ; regs_next
    ; mems
    ; consts
    ; regs_nodes = compute_reg_nodes regs
    ; regs_next_nodes = compute_reg_nodes regs_next
    ; map
    }
  ;;

  let total_size_bytes
    { comb; regs; regs_next; mems; consts; regs_nodes = _; regs_next_nodes = _; map = _ }
    =
    let sum_list l ~f = List.sum (module Int) l ~f in
    let section_size (section : section) = section.size_bytes in
    let reg_section_size sections =
      Clock_domain.Table.fold sections ~init:0 ~f:(fun sum section ->
        sum + section_size section)
    in
    let flat_section_size = sum_list [ comb; mems; consts ] ~f:section_size in
    let reg_section_size = sum_list [ regs; regs_next ] ~f:reg_section_size in
    flat_section_size + reg_section_size
  ;;

  let find_exn (t : t) signal = Map.find_exn t.map (Signal.uid signal)
  let find_address t signal = (find_exn t signal).word_address
end

module Runtime = struct
  type t =
    { in_ports : Port.t list
    ; out_ports_before : Port.t list
    ; out_ports_after : Port.t list
    ; allocation : Nodes_and_addresses.t
    ; clocks : Clocks.t
    ; bytes : Bytes.t
    }
end

module Compiled_updates = struct
  type t =
    { init_consts : (int * Int64.t array) array
    ; reset_consts : (int * Int64.t array) array
    ; startup_consts : (int * Int64.t array) array
    ; comb : unit -> unit
    ; comb_last_layer : unit -> unit
    ; clock_update : unit -> unit
    ; reg_update : unit -> unit
    ; mem_update : unit -> unit
    }
end

type t =
  { runtime : Runtime.t
  ; compiled : Compiled_updates.t
  }

(* Create nodes with addresses for each signal in the circuit. *)
let allocate_addresses (schedule : Signal.t list) clocks =
  List.fold
    schedule
    ~init:(Nodes_and_addresses.Builder.empty clocks)
    ~f:(fun acc signal ->
      match signal with
      | Empty -> acc
      | Const _ -> Nodes_and_addresses.Builder.add_const acc signal (num_words signal)
      | Reg { register; _ } ->
        let clock = Clocks.find_clock_exn clocks register.clock.clock in
        Nodes_and_addresses.Builder.add_reg acc signal clock (num_words signal)
      | Multiport_mem mem ->
        let num_words = num_words signal in
        let mem_size_in_words = num_words * mem.size in
        Nodes_and_addresses.Builder.add_mem acc signal mem_size_in_words
      | Wire { driver = Some _; _ } -> Nodes_and_addresses.Builder.add_wire acc signal
      | _ -> Nodes_and_addresses.Builder.add_comb acc signal (num_words signal))
  |> Nodes_and_addresses.build
;;

let port allocation signal =
  let name = port_name signal in
  let width = Signal.width signal in
  { Port.address = Nodes_and_addresses.find_address allocation signal
  ; name
  ; width
  ; bits = ref (Bits.zero width)
  }
;;

let allocate circuit (config : Config.t) =
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
  let clocks = Clocks.create circuit config.clock_mode in
  let allocation = allocate_addresses schedule clocks in
  let bytes = Bytes.make (Nodes_and_addresses.total_size_bytes allocation) '\000' in
  let in_ports = List.map (Circuit.inputs circuit) ~f:(port allocation) in
  let out_ports_before = List.map (Circuit.outputs circuit) ~f:(port allocation) in
  let out_ports_after = List.map (Circuit.outputs circuit) ~f:(port allocation) in
  { Runtime.in_ports; out_ports_before; out_ports_after; allocation; clocks; bytes }
;;

(* Create a static array of addresses and values of constants. *)
let init_consts (consts : Node.t list) =
  List.map consts ~f:(fun const ->
    match const.signal with
    | Const { constant; _ } ->
      let size_in_words = num_words const.signal in
      let constant = Array.init size_in_words ~f:(Bits.unsafe_get_int64 constant) in
      const.word_address, constant
    | _ -> raise_s [%message "[init_consts] unexpected signal" (const : Node.t)])
  |> Array.of_list
;;

let reset_consts (regs : Node.t list) =
  List.filter_map regs ~f:(fun reg ->
    match reg.signal with
    | Reg { register = { reset; _ }; _ } ->
      Option.map reset ~f:(fun { reset = _; reset_edge = _; reset_to } ->
        if Signal.Type.is_const reset_to
        then (
          let constant = Signal.to_constant reset_to |> Bits.of_constant in
          let size_in_words = num_words reg.signal in
          let reset = Array.init size_in_words ~f:(Bits.unsafe_get_int64 constant) in
          reg.word_address, reset)
        else raise_s [%message "Reset values must be constants" (reg : Node.t)])
    | _ -> raise_s [%message "[reset_consts] expecting reg"])
  |> Array.of_list
;;

let startup_consts (regs : Node.t list) =
  List.filter_map regs ~f:(fun reg ->
    match reg.signal with
    | Reg { register = { initialize_to; _ }; _ } ->
      Option.map initialize_to ~f:(fun initialize_to ->
        let size_in_words = num_words reg.signal in
        let reset = Array.init size_in_words ~f:(Bits.unsafe_get_int64 initialize_to) in
        reg.word_address, reset)
    | _ -> raise_s [%message "[startup_consts] expecting reg"])
  |> Array.of_list
;;

let mem_consts (mems : Node.t list) =
  List.filter_map mems ~f:(fun mem ->
    match mem.signal with
    | Multiport_mem { initialize_to; _ } ->
      Option.map initialize_to ~f:(fun initialize_to ->
        ( mem.word_address
        , List.init (Array.length initialize_to) ~f:(fun address ->
            let constant = initialize_to.(address) in
            let size_in_words = num_words mem.signal in
            let reset = Array.init size_in_words ~f:(Bits.unsafe_get_int64 constant) in
            reset)
          |> Array.concat ))
    | _ -> raise_s [%message "[mem_consts] expecting mem"])
  |> Array.of_list
;;

let memory_size (memory : Signal.t) =
  match memory with
  | Multiport_mem { size; _ } -> size
  | _ -> raise_s [%message "Mem_read_port must reference Multiport_mem"]
;;

let find_exn (t : Runtime.t) signal = Nodes_and_addresses.find_exn t.allocation signal

let find_address (t : Runtime.t) signal =
  Nodes_and_addresses.find_address t.allocation signal
;;

(* Generate code for each combinational signal in the circuit. Excludes registers,
   constants and memories. *)
let compile_comb
  (t : Runtime.t)
  (config : Config.t)
  ({ word_address = dst_address; signal = dst_signal } : Node.t)
  =
  let dst_width = Signal.width dst_signal in
  match dst_signal with
  | Op2 { arg_a; arg_b; op = Add; _ } ->
    let src_address_a = find_address t arg_a in
    let src_address_b = find_address t arg_b in
    let width_in_bits = dst_width in
    Some
      (Cyclesim_ops.add t.bytes ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Sub; _ } ->
    let src_address_a = find_address t arg_a in
    let src_address_b = find_address t arg_b in
    let width_in_bits = dst_width in
    Some
      (Cyclesim_ops.sub t.bytes ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Mulu; _ } ->
    let arg_a = find_exn t arg_a in
    let arg_b = find_exn t arg_b in
    let src_address_a, width_in_bits_a = arg_a.word_address, Signal.width arg_a.signal in
    let src_address_b, width_in_bits_b = arg_b.word_address, Signal.width arg_b.signal in
    Some
      (Cyclesim_ops.mulu
         t.bytes
         ~dst_address
         ~src_address_a
         ~src_address_b
         ~width_in_bits_a
         ~width_in_bits_b)
  | Op2 { arg_a; arg_b; op = Muls; _ } ->
    let arg_a = find_exn t arg_a in
    let arg_b = find_exn t arg_b in
    let src_address_a, width_in_bits_a = arg_a.word_address, Signal.width arg_a.signal in
    let src_address_b, width_in_bits_b = arg_b.word_address, Signal.width arg_b.signal in
    Some
      (Cyclesim_ops.muls
         t.bytes
         ~dst_address
         ~src_address_a
         ~src_address_b
         ~width_in_bits_a
         ~width_in_bits_b)
  | Op2 { arg_a; arg_b; op = And; _ } ->
    let src_address_a = find_address t arg_a in
    let src_address_b = find_address t arg_b in
    let width_in_bits = dst_width in
    Some
      (Cyclesim_ops.and_
         t.bytes
         ~dst_address
         ~src_address_a
         ~src_address_b
         ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Or; _ } ->
    let src_address_a = find_address t arg_a in
    let src_address_b = find_address t arg_b in
    let width_in_bits = dst_width in
    Some
      (Cyclesim_ops.or_ t.bytes ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Xor; _ } ->
    let src_address_a = find_address t arg_a in
    let src_address_b = find_address t arg_b in
    let width_in_bits = dst_width in
    Some
      (Cyclesim_ops.xor t.bytes ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Eq; _ } ->
    let { Node.word_address = src_address_a; signal = arg_a } = find_exn t arg_a in
    let src_address_b = find_address t arg_b in
    let width_in_bits = Signal.width arg_a in
    Some
      (Cyclesim_ops.eq t.bytes ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Op2 { arg_a; arg_b; op = Lt; _ } ->
    let { Node.word_address = src_address_a; signal = arg_a } = find_exn t arg_a in
    let src_address_b = find_address t arg_b in
    let width_in_bits = Signal.width arg_a in
    Some
      (Cyclesim_ops.lt t.bytes ~dst_address ~src_address_a ~src_address_b ~width_in_bits)
  | Not { arg; _ } ->
    let src_address = find_address t arg in
    let width_in_bits = dst_width in
    Some (Cyclesim_ops.not_ t.bytes ~dst_address ~src_address ~width_in_bits)
  | Mux { select; cases; _ } ->
    let select_address = find_address t select in
    let cases = List.map cases ~f:(find_address t) |> Array.of_list in
    let size_in_words = num_words dst_signal in
    let select_width = Signal.width select in
    Some
      (Cyclesim_ops.mux
         t.bytes
         ~dst_address
         ~select_address
         ~select_width
         ~cases
         ~size_in_words)
  | Cases { select; cases; default; _ } ->
    let select_address = find_address t select in
    let select_size_in_words = num_words select in
    let default_address = find_address t default in
    let cases =
      Array.of_list_map cases ~f:(fun (match_with, value) ->
        { Bits_packed.Case.match_with_address = find_address t match_with
        ; value_address = find_address t value
        })
    in
    let value_size_in_words = num_words dst_signal in
    Some
      (Cyclesim_ops.cases
         t.bytes
         ~dst_address
         ~select_address
         ~select_size_in_words
         ~default_address
         ~cases
         ~value_size_in_words)
  | Cat { args; _ } ->
    let cat_src =
      List.map args ~f:(fun arg ->
        let { Node.word_address; signal } = find_exn t arg in
        { Bits_packed.Cat_src.address = word_address; width = Signal.width signal })
    in
    let width_in_bits = dst_width in
    Some (Cyclesim_ops.cat t.bytes ~dst_address cat_src ~width_in_bits)
  | Select { arg; high; low; _ } ->
    let { Node.word_address = src_address; signal = _ } = find_exn t arg in
    Some (Cyclesim_ops.select t.bytes ~dst_address ~src_address ~high ~low)
  | Wire { driver = Some _; _ } ->
    raise_s
      [%message "[compile_comb] driven wires should be removed from the simulator model"]
  | Wire { driver = None; _ } -> None
  | Mem_read_port { memory; read_address; _ } ->
    let size_in_words = num_words dst_signal in
    let read_address = find_address t read_address in
    let memory_address = find_address t memory in
    let memory_size = memory_size memory in
    Some
      (Cyclesim_ops.mem_read
         t.bytes
         ~dst_address
         ~read_address
         ~memory_address
         ~memory_size
         ~size_in_words)
  | Inst { instantiation = i; _ } ->
    (match
       Combinational_ops_database.find
         config.combinational_ops_database
         ~name:i.circuit_name
     with
     | None ->
       raise_s
         [%message
           "Instantiation not supported in simulation" ~name:(i.circuit_name : string)]
     | Some op ->
       let f = Combinational_op.create_fn op in
       let inputs =
         List.map i.inputs ~f:(fun { name = _; input_signal = signal } ->
           Bits.Mutable.create (Signal.width signal))
       in
       let copy_inputs =
         List.map2_exn i.inputs inputs ~f:(fun { name = _; input_signal = signal } bits ->
           let src_address = find_address t signal in
           let size_in_words = num_words signal in
           fun () ->
             for i = 0 to size_in_words - 1 do
               Bits.Mutable.set_int64
                 bits
                 i
                 (Cyclesim_ops.get64 t.bytes (src_address + i))
             done)
       in
       let outputs =
         List.map
           i.outputs
           ~f:(fun { name = _; output_width = bits; output_low_index = _ } ->
             Bits.Mutable.create bits)
       in
       let target = Bits.Mutable.create dst_width in
       let copy_output =
         let size_in_words = num_words dst_signal in
         fun () ->
           for i = 0 to size_in_words - 1 do
             Cyclesim_ops.set64
               t.bytes
               (dst_address + i)
               (Bits.Mutable.get_int64 target i)
           done
       in
       (* The reversed order is is needed because of the msb first concat below. *)
       let outputs_in_reverse_order = List.rev outputs in
       let f = Staged.unstage (f ~inputs ~outputs) in
       let inst () =
         List.iter copy_inputs ~f:(fun f -> f ());
         f ();
         Bits.Mutable.concat target outputs_in_reverse_order;
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

let compile_clock_update (t : Runtime.t) : (unit -> unit) list =
  match t.clocks with
  | Single_domain _ -> []
  | Multi_domain clocks ->
    let update_fns =
      Clock_domain.Table.init clocks.domains ~f:(fun indexed ->
        let name = (Clock_domain.domain indexed).name |> Clock_domain.Name.to_string in
        let in_port =
          match List.find t.in_ports ~f:(fun port -> String.equal name port.name) with
          | Some port -> port
          | None ->
            raise_s [%message "No input port matching clock domain" (name : string)]
        in
        fun value -> in_port.bits := value)
    in
    Iarray.to_list (Clock_domain.Group.elements clocks.domains)
    |> List.map ~f:(fun clock ->
      let update_fn = Clock_domain.Table.get update_fns clock in
      let period = (Clock_domain.domain clock).period in
      let half_period = period / 2 in
      fun () ->
        match clocks.cycle with
        | Not_started -> ()
        | Cycle cycle ->
          if cycle % period = 0
          then update_fn Bits.vdd
          else if cycle % half_period = 0
          then update_fn Bits.gnd)
;;

(* Generate code for updating registers. *)
let compile_reg_update (t : Runtime.t) (dst : Node.t) : unit -> unit =
  let size_in_words = num_words dst.signal in
  match dst.signal with
  | Reg { register; d; _ } ->
    let dst_address = dst.word_address in
    let src_address = find_address t d in
    let clear =
      Option.map register.clear ~f:(fun { clear; clear_to } ->
        { Cyclesim_ops.clear = find_address t clear
        ; clear_value = find_address t clear_to
        })
    in
    let enable = Option.map register.enable ~f:(find_address t) in
    let update_fn =
      Cyclesim_ops.reg t.bytes ~clear ~enable ~dst_address ~src_address ~size_in_words
    in
    (match t.clocks with
     | Single_domain _ -> update_fn
     | Multi_domain clocks ->
       let clock = Clocks.Multi_domain.find_clock_exn clocks register.clock.clock in
       fun () ->
         if Clock_domain.Set.mem clocks.clocks_at_edge_this_cycle clock then update_fn ())
  | _ -> raise_s [%message "[compile_reg_update] expecting reg"]
;;

let compile_mem_update (t : Runtime.t) (dst : Node.t) : unit -> unit =
  match dst.signal with
  | Multiport_mem { size; write_ports; _ } ->
    let size_in_words = num_words dst.signal in
    let memory_address = find_address t dst.signal in
    let port_updates =
      Array.map
        write_ports
        ~f:(fun { write_clock; write_enable; write_address; write_data } ->
          let write_enable = find_address t write_enable in
          let write_address = find_address t write_address in
          let write_data = find_address t write_data in
          let update_fn =
            Cyclesim_ops.mem_write_port
              t.bytes
              ~size
              ~memory_address
              ~write_enable
              ~write_address
              ~write_data
              ~size_in_words
          in
          match t.clocks with
          | Single_domain _ -> update_fn
          | Multi_domain clocks ->
            let clock = Clocks.Multi_domain.find_clock_exn clocks write_clock in
            fun () ->
              if Clock_domain.Set.mem clocks.clocks_at_edge_this_cycle clock
              then update_fn ())
    in
    fun () -> Array.iter port_updates ~f:(fun f -> f ())
  | _ -> raise_s [%message "[compile_mem_update] expecting memory"]
;;

let last_layer circuit (t : Runtime.t) =
  let nodes =
    Signal_graph.last_layer_of_nodes
      ~is_input:(Circuit.is_input circuit)
      (Circuit.signal_graph circuit)
    |> Set.of_list (module Signal.Type.Uid)
  in
  List.filter t.allocation.comb.nodes ~f:(fun alloc ->
    Set.mem nodes (Signal.uid alloc.signal))
;;

let copy_in_ports t (ports : Port.t list) =
  List.iter ports ~f:(fun { name = _; address; width = _; bits } ->
    let bytes = Bits.Expert.unsafe_underlying_repr !bits in
    let header_size = Bits.Expert.offset_for_data in
    Bytes.unsafe_blit
      ~src:bytes
      ~src_pos:header_size
      ~dst:t.runtime.bytes
      ~dst_pos:(address * 8)
      ~len:(Bytes.length bytes - header_size))
;;

let copy_out_ports t (ports : Port.t list) =
  List.iter ports ~f:(fun { name = _; address; width; bits } ->
    let new_bits = Bits.zero width in
    let bytes = Bits.Expert.unsafe_underlying_repr new_bits in
    let header_size = Bits.Expert.offset_for_data in
    Bytes.unsafe_blit
      ~src:t.runtime.bytes
      ~src_pos:(address * 8)
      ~dst:bytes
      ~dst_pos:header_size
      ~len:(Bytes.length bytes - header_size);
    bits := new_bits)
;;

let lookup_node_by_id t signal_id =
  Map.find t.runtime.allocation.map signal_id
  |> Option.map ~f:(fun { word_address; signal } ->
    Cyclesim0.Node.create_from_signal
      ~byte_address:(word_address * 8)
      ~data:t.runtime.bytes
      signal)
;;

let lookup_node t (traced : Cyclesim0.Traced.internal_signal) =
  if not (Signal.Type.is_mem traced.signal || Signal.Type.is_reg traced.signal)
  then lookup_node_by_id t (Signal.uid traced.signal)
  else None
;;

let lookup_reg_by_id t signal_id =
  Map.find t.runtime.allocation.map signal_id
  |> Option.map ~f:(fun { word_address; signal } ->
    Cyclesim0.Reg.create_from_signal
      ~byte_address:(word_address * 8)
      ~data:t.runtime.bytes
      signal)
;;

let lookup_reg t (traced : Cyclesim0.Traced.internal_signal) =
  if Signal.Type.is_reg traced.signal
  then lookup_reg_by_id t (Signal.uid traced.signal)
  else None
;;

let lookup_mem t (traced : Cyclesim0.Traced.internal_signal) =
  if Signal.Type.is_mem traced.signal
  then
    Map.find t.runtime.allocation.map (Signal.uid traced.signal)
    |> Option.map ~f:(fun { word_address; _ } ->
      Cyclesim0.Memory.create_from_signal
        ~byte_address:(word_address * 8)
        ~data:t.runtime.bytes
        traced.signal)
  else None
;;

let create_cyclesim circuit t (traced : Cyclesim0.Traced.t) =
  let%tydi { runtime =
               { in_ports
               ; out_ports_before
               ; out_ports_after
               ; allocation
               ; clocks
               ; bytes
               ; _
               }
           ; compiled =
               { init_consts
               ; reset_consts
               ; startup_consts
               ; comb
               ; comb_last_layer
               ; clock_update
               ; reg_update
               ; mem_update
               }
           }
    =
    t
  in
  (* Initialize constant signals *)
  let set_consts consts =
    Array.iter consts ~f:(fun (address, data) ->
      Array.iteri data ~f:(fun i data ->
        Bytes.unsafe_set_int64 bytes ((address + i) * 8) data))
  in
  let reset_clocks () = Clocks.reset clocks in
  let is_true_cycle side = Clocks.is_true_cycle clocks side in
  set_consts init_consts;
  set_consts startup_consts;
  reset_clocks ();
  let reset () =
    set_consts reset_consts;
    reset_clocks ()
  in
  (* Simulation steps *)
  let cycle_check () =
    if is_true_cycle `Before_edge
    then
      List.iter in_ports ~f:(fun { Port.name; address = _; width; bits } ->
        if Bits.width !bits <> width
        then
          raise_s
            [%message
              "Invalid input port width" (name : string) (width : int) (bits : Bits.t ref)])
  in
  let cycle_before_clock_edge () =
    Clocks.incr_cycle clocks;
    clock_update ();
    if is_true_cycle `Before_edge
    then (
      (* copy in inputs *)
      copy_in_ports t in_ports;
      (* main combinational step *)
      comb ();
      (* grab outputs before clock edge *)
      copy_out_ports t out_ports_before)
  in
  let cycle_at_clock_edge () =
    if is_true_cycle `At_edge
    then (
      (* perform register updates *)
      reg_update ();
      (* run memory writes *)
      mem_update ();
      (* copy back new register values *)
      Clock_domain.Set.iter (Clocks.clocks_this_cycle clocks `At_edge) ~f:(fun clock ->
        let regs = Clock_domain.Table.get allocation.regs clock in
        let regs_next = Clock_domain.Table.get allocation.regs_next clock in
        Bytes.blito
          ~dst:bytes
          ~dst_pos:regs.byte_address
          ~src:bytes
          ~src_pos:regs_next.byte_address
          ~src_len:regs.size_bytes
          ()))
  in
  let cycle_after_clock_edge () =
    if is_true_cycle `At_edge
    then (
      (* update combinational outputs wrt to new register values *)
      comb_last_layer ();
      (* copy output ports *)
      copy_out_ports t out_ports_after)
  in
  let get_ports (ports : Port.t list) =
    List.map ports ~f:(fun { name; address = _; width = _; bits } -> name, bits)
  in
  let clocks_aligned () = Clocks.aligned clocks in
  Cyclesim0.Private.create
    ?circuit
    ~in_ports:(get_ports in_ports)
    ~out_ports_before_clock_edge:(get_ports out_ports_before)
    ~out_ports_after_clock_edge:(get_ports out_ports_after)
    ~reset
    ~clock_mode:
      (match clocks with
       | Single_domain _ -> `All_one_domain
       | Multi_domain _ -> `By_input_clocks)
    ~clocks_aligned
    ~cycle_multiple:(Clocks.cycle_multiple clocks)
    ~cycle_check
    ~cycle_before_clock_edge
    ~cycle_at_clock_edge
    ~cycle_after_clock_edge
    ~traced
    ~lookup_node_by_id:(lookup_node_by_id t)
    ~lookup_node:(lookup_node t)
    ~lookup_reg_by_id:(lookup_reg_by_id t)
    ~lookup_reg:(lookup_reg t)
    ~lookup_mem:(lookup_mem t)
    ()
;;

let compile circuit (t : Runtime.t) config =
  let init_consts = init_consts t.allocation.consts.nodes in
  let reset_consts =
    (* We must reset both the current and next values - if the register is not enabled in
       the subsequent cycle it wont change the 'reg_next' value - and the reset gets lost.
    *)
    Array.concat
      [ reset_consts t.allocation.regs_nodes; reset_consts t.allocation.regs_next_nodes ]
  in
  let startup_consts =
    Array.concat
      [ startup_consts t.allocation.regs_nodes
      ; startup_consts t.allocation.regs_next_nodes
      ; mem_consts t.allocation.mems.nodes
      ]
  in
  let comb = List.filter_map t.allocation.comb.nodes ~f:(compile_comb t config) in
  let comb_last_layer =
    List.filter_map (last_layer circuit t) ~f:(compile_comb t config)
  in
  let clock_update = compile_clock_update t in
  let reg_update = List.map t.allocation.regs_next_nodes ~f:(compile_reg_update t) in
  let mem_update = List.map t.allocation.mems.nodes ~f:(compile_mem_update t) in
  let run tasks =
    let tasks = Array.of_list tasks in
    let len = Array.length tasks in
    fun () ->
      for i = 0 to len - 1 do
        (Array.unsafe_get tasks i) ()
      done
  in
  { Compiled_updates.init_consts
  ; reset_consts
  ; startup_consts
  ; comb = run comb
  ; comb_last_layer = run comb_last_layer
  ; clock_update = run clock_update
  ; reg_update = run reg_update
  ; mem_update = run mem_update
  }
;;

let pseudorandomly_initialize_register ~random_state ~bytes (register_node : Node.t) =
  let reg =
    Cyclesim0.Reg.create_from_signal
      ~byte_address:(register_node.word_address * 8)
      ~data:bytes
      register_node.signal
  in
  let width = Cyclesim0.Reg.width_in_bits reg in
  let bits = Bits.Mutable.create width in
  Bits.Mutable.randomize ~random_state bits;
  Cyclesim0.Reg.of_bits_mutable reg bits
;;

let pseudorandomly_initialize_memories ~random_state ~bytes (memory_node : Node.t) =
  let mem =
    Cyclesim0.Memory.create_from_signal
      ~byte_address:(memory_node.word_address * 8)
      ~data:bytes
      memory_node.signal
  in
  let width = Cyclesim0.Memory.width_in_bits mem in
  let bits = Bits.Mutable.create width in
  for address = 0 to Cyclesim0.Memory.memory_size mem - 1 do
    Bits.Mutable.randomize ~random_state bits;
    Cyclesim0.Memory.of_bits_mutable ~address mem bits
  done
;;

let initialize_state
  ~bytes
  ~(allocation : Nodes_and_addresses.t)
  ~random_initializer:
    ({ random_state; initialize } : Cyclesim0.Config.Random_initializer.t)
  =
  List.iter
    ~f:(fun reg ->
      if initialize reg.signal
      then pseudorandomly_initialize_register ~random_state ~bytes reg)
    allocation.regs_nodes;
  List.iter
    ~f:(fun reg ->
      if initialize reg.signal
      then pseudorandomly_initialize_register ~random_state ~bytes reg)
    allocation.regs_next_nodes;
  List.iter
    ~f:(fun mem ->
      if initialize mem.signal
      then pseudorandomly_initialize_memories ~random_state ~bytes mem)
    allocation.mems.nodes
;;

let create ?(config = Cyclesim0.Config.default) circuit =
  let circuit =
    if config.deduplicate_signals then Dedup.deduplicate circuit else circuit
  in
  let traced =
    Cyclesim0.Traced_nodes.create circuit ~is_internal_port:config.is_internal_port
  in
  let runtime = allocate circuit config in
  let compiled = compile circuit runtime config in
  Option.iter config.random_initializer ~f:(fun random_initializer ->
    initialize_state
      ~bytes:runtime.bytes
      ~random_initializer
      ~allocation:runtime.allocation);
  create_cyclesim
    (if config.store_circuit then Some circuit else None)
    { runtime; compiled }
    traced
;;
