open Base

module type Private = sig
  type ('i, 'o) t
  type port_list
  type t_port_list
  type task = unit -> unit

  val create
    :  ?circuit:Circuit.t
    -> in_ports:port_list
    -> out_ports_before_clock_edge:port_list
    -> out_ports_after_clock_edge:port_list
    -> internal_ports:port_list
    -> reset:task
    -> cycle_check:task
    -> cycle_before_clock_edge:task
    -> cycle_at_clock_edge:task
    -> cycle_after_clock_edge:task
    -> lookup_reg:(string -> Bits.Mutable.t option)
    -> lookup_mem:(string -> Bits.Mutable.t array option)
    -> assertions:Bits.Mutable.t Map.M(String).t
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

  module Digest : sig
    type t = Md5_lib.t [@@deriving sexp_of, compare, equal]

    val none : t
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
    ; lookup_reg : string -> Bits.Mutable.t option
    ; lookup_mem : string -> Bits.Mutable.t array option
    ; assertions : Bits.Mutable.t Map.M(String).t
    ; violated_assertions : int list Hashtbl.M(String).t
    ; digest : Digest.t ref
    ; circuit : Circuit.t option
    }
  [@@deriving fields, sexp_of]

  type t_port_list = (Port_list.t, Port_list.t) t

  module Config : sig
    type t =
      { is_internal_port : (Signal.t -> bool) option
      (** Passed each signal in the design which has a name. Returns [true] if the
          simulator should expose it for reading in the testbench (or display in a
          waveform). *)
      ; combinational_ops_database : Combinational_ops_database.t
      (** Database of instantiations which may be replace by a combinational operation. *)
      ; compute_digest : bool
      (** Compute an md5 digest of the outputs of a simulation run. Enabled by default
          within inlined tests. *)
      ; deduplicate_signals : bool
      (** Perform a pass which finds structurally equal signals and shares them. *)
      ; store_circuit : bool
      (** Stores the post-processed circuit that is used to compile the
          simulation. This should generally be set to false, so that the Circuit
          can be garbage collected once the simulation is constructed.
      *)
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
