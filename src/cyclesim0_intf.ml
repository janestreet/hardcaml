open Base

module type Private = sig
  type ('i, 'o) t
  type port_list
  type t_port_list
  type task = unit -> unit

  val create
    :  in_ports:port_list
    -> out_ports_before_clock_edge:port_list
    -> out_ports_after_clock_edge:port_list
    -> internal_ports:port_list
    -> reset:task
    -> cycle_check:task
    -> cycle_before_clock_edge:task
    -> cycle_at_clock_edge:task
    -> cycle_after_clock_edge:task
    -> lookup_signal:(Signal.Uid.t -> Bits.t ref)
    -> lookup_reg:(Signal.Uid.t -> Bits.t ref)
    -> assertions:Signal.t Map.M(String).t
    -> t_port_list

  module Step : sig
    type t =
      | Reset
      | Check
      | Before_clock_edge
      | At_clock_edge
      | After_clock_edge
    [@@deriving sexp_of]
  end

  val modify : ('i, 'o) t -> (Side.t * Step.t * task) list -> ('i, 'o) t

  val coerce
    :  (port_list, port_list) t
    -> to_input:(port_list -> 'i)
    -> to_output:(port_list -> 'o)
    -> ('i, 'o) t
end

module type Cyclesim0 = sig
  module Port_list : sig
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
  [@@deriving fields, sexp_of]

  type t_port_list = (Port_list.t, Port_list.t) t

  module Config : sig
    type t =
      { is_internal_port : (Signal.t -> bool) option
      ; combinational_ops_database : Combinational_ops_database.t
      }

    val default : t
    val trace : bool -> t
    val trace_all : t
  end

  module type Private = Private

  module Private :
    Private
    with type ('i, 'o) t = ('i, 'o) t
     and type port_list = Port_list.t
     and type t_port_list = t_port_list
end
