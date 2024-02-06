open Base

module type Private = sig
  type ('i, 'o) t
  type port_list
  type t_port_list
  type traced
  type traced_io_port
  type traced_internal_signal
  type node
  type reg
  type memory
  type task = unit -> unit

  val create
    :  ?circuit:Circuit.t
    -> in_ports:port_list
    -> out_ports_before_clock_edge:port_list
    -> out_ports_after_clock_edge:port_list
    -> reset:task
    -> cycle_check:task
    -> cycle_before_clock_edge:task
    -> cycle_at_clock_edge:task
    -> cycle_after_clock_edge:task
    -> traced:traced
    -> lookup_node:(traced_internal_signal -> node option)
    -> lookup_reg:(traced_internal_signal -> reg option)
    -> lookup_mem:(traced_internal_signal -> memory option)
    -> unit
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

  module Traced : sig
    type io_port =
      { signal : Signal.t
      ; name : string
      }
    [@@deriving sexp_of]

    type internal_signal =
      { signal : Signal.t
      ; mangled_names : string list
      }
    [@@deriving sexp_of]

    type t =
      { input_ports : io_port list
      ; output_ports : io_port list
      ; internal_signals : internal_signal list
      }
    [@@deriving sexp_of]

    val to_io_port : Signal.t -> io_port
  end

  module Node = Cyclesim_lookup.Node
  module Reg = Cyclesim_lookup.Reg
  module Memory = Cyclesim_lookup.Memory

  type task = unit -> unit

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
  [@@deriving fields ~getters, sexp_of]

  type t_port_list = (Port_list.t, Port_list.t) t

  module Config : sig
    type t =
      { is_internal_port : (Signal.t -> bool) option
          (** Passed each signal in the design which has a name. Returns [true] if the
          simulator should expose it for reading in the testbench (or display in a
          waveform). *)
      ; combinational_ops_database : Combinational_ops_database.t
          (** Database of instantiations which may be replace by a combinational operation. *)
      ; deduplicate_signals : bool
          (** Perform a pass which finds structurally equal signals and shares them. *)
      ; store_circuit : bool
          (** Stores the post-processed circuit that is used to compile the
          simulation. This should generally be set to false, so that the Circuit
          can be garbage collected once the simulation is constructed.
      *)
      }

    val default : t
    val trace : [ `Everything | `All_named | `Ports_only ] -> t
    val trace_all : t
  end

  module type Private = Private

  module Private :
    Private
      with type ('i, 'o) t = ('i, 'o) t
       and type port_list = Port_list.t
       and type t_port_list = t_port_list
       and type traced = Traced.t
       and type traced_io_port = Traced.io_port
       and type traced_internal_signal = Traced.internal_signal
       and type node = Node.t
       and type reg = Reg.t
       and type memory = Memory.t
end
