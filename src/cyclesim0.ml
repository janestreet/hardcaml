open Base

module Port_list = struct
  type t = (string * Bits.t ref) list [@@deriving sexp_of]
end

type task = unit -> unit

type ('i, 'o) t =
  { in_ports : Port_list.t
  ; out_ports_before_clock_edge : Port_list.t
  ; out_ports_after_clock_edge : Port_list.t
  ; internal_ports : Port_list.t
  ; inputs : 'i
  ; outputs_after_clock_edge : 'o
  ; outputs_before_clock_edge : 'o
  ; reset : task
  ; cycle_check : task
  ; cycle_before_clock_edge : task
  ; cycle_at_clock_edge : task
  ; cycle_after_clock_edge : task
  ; lookup_signal : Signal.Uid.t -> Bits.t ref
  ; lookup_reg : Signal.Uid.t -> Bits.t ref
  ; assertions : Signal.t Map.M(String).t
  ; violated_assertions : int list Hashtbl.M(String).t
  }
[@@deriving fields]

let sexp_of_t sexp_of_i sexp_of_o t =
  [%message
    ""
      ~inputs:(t.inputs : i)
      ~outputs_before_clock_edge:(t.outputs_before_clock_edge : o)
      ~outputs_after_clock_edge:(t.outputs_after_clock_edge : o)]
;;

type t_port_list = (Port_list.t, Port_list.t) t

module type Private = Cyclesim0_intf.Private

module Private = struct
  type nonrec ('i, 'o) t = ('i, 'o) t
  type nonrec port_list = Port_list.t
  type nonrec t_port_list = t_port_list
  type nonrec task = task

  let create
        ~in_ports
        ~out_ports_before_clock_edge
        ~out_ports_after_clock_edge
        ~internal_ports
        ~reset
        ~cycle_check
        ~cycle_before_clock_edge
        ~cycle_at_clock_edge
        ~cycle_after_clock_edge
        ~lookup_signal
        ~lookup_reg
        ~assertions
    =
    { in_ports
    ; out_ports_before_clock_edge
    ; out_ports_after_clock_edge
    ; internal_ports
    ; inputs = in_ports
    ; outputs_before_clock_edge = out_ports_before_clock_edge
    ; outputs_after_clock_edge = out_ports_after_clock_edge
    ; reset
    ; cycle_check
    ; cycle_before_clock_edge
    ; cycle_at_clock_edge
    ; cycle_after_clock_edge
    ; lookup_signal
    ; lookup_reg
    ; assertions
    ; violated_assertions = Hashtbl.create (module String)
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
