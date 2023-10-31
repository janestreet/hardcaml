open Base

module Port_list = struct
  type t = (string * Bits.t ref) list [@@deriving sexp_of]
end

module Traced = struct
  type t =
    { signal : Signal.t
    ; names : string list
    }

  let sexp_of_t { signal; names } =
    let sexp_of_signal = Signal.sexp_of_signal_recursive ~show_uids:false ~depth:1 in
    [%message (signal : signal) (names : string list)]
  ;;
end

type task = unit -> unit

module Digest = struct
  type t = Md5_lib.t

  let sexp_of_t t = [%sexp_of: string] (Md5_lib.to_hex t : string)
  let compare a b = String.compare (Md5_lib.to_binary a) (Md5_lib.to_binary b)
  let equal a b = String.equal (Md5_lib.to_binary a) (Md5_lib.to_binary b)
  let none = Md5_lib.string "none"
end

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
  ; traced : Traced.t list
  ; lookup : Signal.t -> Bits.Mutable.t option
  ; lookup_reg : string -> Bits.Mutable.t option
  ; lookup_mem : string -> Bits.Mutable.t array option
  ; assertions : Bits.Mutable.t Map.M(String).t
  ; violated_assertions : int list Hashtbl.M(String).t
  ; digest : Digest.t ref
  ; circuit : Circuit.t option
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
type traced = Traced.t list

module Config = struct
  type t =
    { is_internal_port : (Signal.t -> bool) option
    ; combinational_ops_database : Combinational_ops_database.t
    ; compute_digest : bool
    ; deduplicate_signals : bool
    ; store_circuit : bool
    }

  let empty_ops_database = Combinational_ops_database.create ()

  let default =
    { is_internal_port = None
    ; combinational_ops_database = empty_ops_database
    ; compute_digest = Exported_for_specific_uses.am_testing
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
    ; compute_digest = Exported_for_specific_uses.am_testing
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
  type nonrec traced = traced

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
    ~lookup
    ~lookup_reg
    ~lookup_mem
    ~assertions
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
    ; assertions
    ; traced
    ; lookup
    ; lookup_reg
    ; lookup_mem
    ; violated_assertions = Hashtbl.create (module String)
    ; digest = ref (Md5_lib.string "none")
    ; circuit
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
