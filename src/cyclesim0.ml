open Base

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

type ('i, 'o) t =
  { in_ports : Port_list.t
  ; out_ports_before_clock_edge : Port_list.t
  ; out_ports_after_clock_edge : Port_list.t
  ; inputs : 'i
  ; outputs_after_clock_edge : 'o
  ; outputs_before_clock_edge : 'o
  ; reset : task
  ; cycle_check : task
  ; cycle_before_clock_edge : task
  ; cycle_at_clock_edge : task
  ; cycle_after_clock_edge : task
  ; traced : Traced.t
  ; lookup_node : Traced.internal_signal -> Node.t option
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
  type t =
    { is_internal_port : (Signal.t -> bool) option
    ; combinational_ops_database : Combinational_ops_database.t
    ; deduplicate_signals : bool
    ; store_circuit : bool
    }

  let empty_ops_database = Combinational_ops_database.create ()

  let default =
    { is_internal_port = None
    ; combinational_ops_database = empty_ops_database
    ; deduplicate_signals = false
    ; store_circuit = false
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
    }
  ;;

  let trace_all = trace `All_named
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
    ~cycle_check
    ~cycle_before_clock_edge
    ~cycle_at_clock_edge
    ~cycle_after_clock_edge
    ~traced
    ~lookup_node
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
    ; cycle_check
    ; cycle_before_clock_edge
    ; cycle_at_clock_edge
    ; cycle_after_clock_edge
    ; traced
    ; lookup_node
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
