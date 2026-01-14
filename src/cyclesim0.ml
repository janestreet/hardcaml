open! Core0

module Port_list = struct
  type t = (string * Bits.t ref) list [@@deriving sexp_of]
end

module Traced = struct
  type signal = Signal.t

  let sexp_of_signal = Signal.Type.sexp_of_signal_recursive ~show_uids:false ~depth:1

  type io_port =
    { signal : signal
    ; name : string
    }
  [@@deriving sexp_of]

  type internal_signal =
    { signal : signal
    ; mangled_names : string list
    }
  [@@deriving sexp_of]

  type t =
    { input_ports : io_port list
    ; output_ports : io_port list
    ; internal_signals : internal_signal list
    }
  [@@deriving sexp_of]

  let to_io_port signal =
    let name = List.hd_exn (Signal.names signal) in
    { signal; name }
  ;;
end

type task = unit -> unit

module Node = Cyclesim_lookup.Node
module Reg = Cyclesim_lookup.Reg
module Memory = Cyclesim_lookup.Memory

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

type ('i, 'o) t =
  { in_ports : Port_list.t
  ; out_ports_before_clock_edge : Port_list.t
  ; out_ports_after_clock_edge : Port_list.t
  ; inputs : 'i
  ; outputs_after_clock_edge : 'o
  ; outputs_before_clock_edge : 'o
  ; reset : task
  ; clock_mode : [ `All_one_domain | `By_input_clocks ]
  ; clocks_aligned : unit -> bool
  ; cycle_multiple : int
  ; cycle_check : task
  ; cycle_before_clock_edge : task
  ; cycle_at_clock_edge : task
  ; cycle_after_clock_edge : task
  ; traced : Traced.t
  ; lookup_node_by_id : Signal.Type.Uid.t -> Node.t option
  ; lookup_node : Traced.internal_signal -> Node.t option
  ; lookup_reg_by_id : Signal.Type.Uid.t -> Reg.t option
  ; lookup_reg : Traced.internal_signal -> Reg.t option
  ; lookup_mem : Traced.internal_signal -> Memory.t option
  ; circuit : Circuit.t option
  ; node_by_name : Traced.internal_signal Map.M(String).t Lazy.t
  ; memory_by_name : Traced.internal_signal Map.M(String).t Lazy.t
  ; reg_by_name : Traced.internal_signal Map.M(String).t Lazy.t
  }
[@@deriving fields ~getters]

let sexp_of_t sexp_of_i sexp_of_o t =
  [%message
    ""
      ~inputs:(t.inputs : i)
      ~outputs_before_clock_edge:(t.outputs_before_clock_edge : o)
      ~outputs_after_clock_edge:(t.outputs_after_clock_edge : o)]
;;

type t_port_list = (Port_list.t, Port_list.t) t

module Config = struct
  module Random_initializer = struct
    type t =
      { random_state : Splittable_random.t
      ; initialize : Signal.t -> bool
      }

    let create ?random_state initialize =
      { random_state =
          (match random_state with
           | None -> Splittable_random.of_int 0
           | Some r -> r)
      ; initialize
      }
    ;;

    let randomize_regs s = Signal.Type.is_reg s && not (Signal.Type.has_initializer s)
    let randomize_memories s = Signal.Type.is_mem s && not (Signal.Type.has_initializer s)
    let randomize_all s = randomize_regs s || randomize_memories s
  end

  module Clock_mode = struct
    type t =
      | All_one_domain
      | By_input_clocks of Cyclesim_clock_domain.t list
  end

  type t =
    { is_internal_port : (Signal.t -> bool) option
    ; combinational_ops_database : Combinational_ops_database.t
    ; deduplicate_signals : bool
    ; store_circuit : bool
    ; random_initializer : Random_initializer.t option
    ; clock_mode : Clock_mode.t
    }

  let empty_ops_database = Combinational_ops_database.create ()

  let default =
    { is_internal_port = None
    ; combinational_ops_database = empty_ops_database
    ; deduplicate_signals = false
    ; store_circuit = false
    ; random_initializer = None
    ; clock_mode = All_one_domain
    }
  ;;

  let trace how =
    { is_internal_port =
        (match how with
         | `Everything -> Some (Fn.const true)
         | `All_named -> Some (fun s -> not (List.is_empty (Signal.names s)))
         | `Ports_only -> None)
    ; combinational_ops_database = empty_ops_database
    ; deduplicate_signals = false
    ; store_circuit = false
    ; random_initializer = None
    ; clock_mode = All_one_domain
    }
  ;;

  let trace_all = trace `All_named
  let add_random_initialization t i = { t with random_initializer = Some i }
end

module type Private = Cyclesim0_intf.Private

module Private = struct
  type nonrec ('i, 'o) t = ('i, 'o) t
  type nonrec port_list = Port_list.t
  type nonrec t_port_list = t_port_list
  type nonrec task = task
  type node = Node.t
  type reg = Reg.t
  type memory = Memory.t
  type traced = Traced.t
  type traced_internal_signal = Traced.internal_signal
  type traced_io_port = Traced.io_port

  let by_name predicate (traced : Traced.internal_signal list) =
    List.fold
      traced
      ~init:(Map.empty (module String))
      ~f:(fun map ({ signal; mangled_names } as traced) ->
        if predicate signal
        then
          List.fold_left mangled_names ~init:map ~f:(fun map name ->
            Map.set map ~key:name ~data:traced)
        else map)
  ;;

  let node_by_name =
    by_name (fun s -> (not (Signal.Type.is_mem s)) && not (Signal.Type.is_reg s))
  ;;

  let memory_by_name = by_name (fun s -> Signal.Type.is_mem s)
  let reg_by_name = by_name (fun s -> Signal.Type.is_reg s)

  let create
    ?circuit
    ~in_ports
    ~out_ports_before_clock_edge
    ~out_ports_after_clock_edge
    ~reset
    ~clock_mode
    ~clocks_aligned
    ~cycle_multiple
    ~cycle_check
    ~cycle_before_clock_edge
    ~cycle_at_clock_edge
    ~cycle_after_clock_edge
    ~traced
    ~lookup_node_by_id
    ~lookup_node
    ~lookup_reg_by_id
    ~lookup_reg
    ~lookup_mem
    ()
    =
    { in_ports
    ; out_ports_before_clock_edge
    ; out_ports_after_clock_edge
    ; inputs = in_ports
    ; outputs_before_clock_edge = out_ports_before_clock_edge
    ; outputs_after_clock_edge = out_ports_after_clock_edge
    ; reset
    ; clock_mode
    ; clocks_aligned
    ; cycle_multiple
    ; cycle_check
    ; cycle_before_clock_edge
    ; cycle_at_clock_edge
    ; cycle_after_clock_edge
    ; traced
    ; lookup_node_by_id
    ; lookup_node
    ; lookup_reg_by_id
    ; lookup_reg
    ; lookup_mem
    ; circuit
    ; node_by_name = lazy (node_by_name traced.internal_signals)
    ; memory_by_name = lazy (memory_by_name traced.internal_signals)
    ; reg_by_name = lazy (reg_by_name traced.internal_signals)
    }
  ;;

  module Step = struct
    type t =
      | Reset
      | Check
      | Before_clock_edge
      | At_clock_edge
      | After_clock_edge
    [@@deriving sexp_of]
  end

  let modify (t : _ t) l =
    List.fold l ~init:t ~f:(fun t ((side : Side.t), (step : Step.t), f) ->
      let apply current =
        match side with
        | Before ->
          fun () ->
            f ();
            current ()
        | After ->
          fun () ->
            current ();
            f ()
      in
      match step with
      | Reset -> { t with reset = apply t.reset }
      | Check -> { t with cycle_check = apply t.cycle_check }
      | Before_clock_edge ->
        { t with cycle_before_clock_edge = apply t.cycle_before_clock_edge }
      | At_clock_edge -> { t with cycle_at_clock_edge = apply t.cycle_at_clock_edge }
      | After_clock_edge ->
        { t with cycle_after_clock_edge = apply t.cycle_after_clock_edge })
  ;;

  let coerce (sim : _ t) ~to_input ~to_output =
    { sim with
      inputs = to_input sim.in_ports
    ; outputs_after_clock_edge = to_output sim.out_ports_after_clock_edge
    ; outputs_before_clock_edge = to_output sim.out_ports_before_clock_edge
    }
  ;;
end
