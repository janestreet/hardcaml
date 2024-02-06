open Base

module type Cyclesim = sig
  module Port_list = Cyclesim0.Port_list
  module Traced = Cyclesim0.Traced
  module Node = Cyclesim0.Node
  module Reg = Cyclesim0.Reg
  module Memory = Cyclesim0.Memory
  module Config = Cyclesim0.Config

  (** base type of the cycle based simulators *)
  type ('i, 'o) t

  type t_port_list = (Port_list.t, Port_list.t) t

  (** returns the circuit used to compile the simulation. *)
  val circuit : _ t -> Circuit.t option

  (** advance by 1 clock cycle (check->comb->seq->comb) *)
  val cycle : _ t -> unit

  (** check inputs are valid before a simulation cycle *)
  val cycle_check : _ t -> unit

  (** update combinatorial logic before clock edge and relative to new inputs. *)
  val cycle_before_clock_edge : _ t -> unit

  (** update sequential logic - registers and memories. *)
  val cycle_at_clock_edge : _ t -> unit

  (** update combinatorial logic after clock edge *)
  val cycle_after_clock_edge : _ t -> unit

  (** reset simulator *)
  val reset : _ t -> unit

  (** get input port given a name *)
  val in_port : _ t -> string -> Bits.t ref

  (** Signals and their unique (mangled) names to be traced by the simulation. Includes
      both IO ports and internal signals - the latter are accessible via the [lookup]
      function. *)
  val traced : _ t -> Traced.t

  (** Get output port given a name.  If [clock_edge] is [Before] the outputs are computed
      prior to the clock edge - [After] means the outputs are computed after the clock
      edge. *)
  val out_port
    :  ?clock_edge:Side.t (** default is [After]. *)
    -> _ t
    -> string
    -> Bits.t ref

  val inputs : ('i, _) t -> 'i
  val outputs : ?clock_edge:Side.t -> (_, 'o) t -> 'o
  val in_ports : _ t -> Port_list.t
  val out_ports : ?clock_edge:Side.t -> _ t -> Port_list.t

  (** Current value of an internal (combinational) node within the simulator. *)
  val lookup_node : _ t -> Traced.internal_signal -> Node.t option

  val lookup_node_by_name : _ t -> string -> Node.t option
  val lookup_node_or_reg : _ t -> Traced.internal_signal -> Node.t option
  val lookup_node_or_reg_by_name : _ t -> string -> Node.t option

  (** Peek at internal registers, return Some _ if it's present. Note
      that the node must marked as traced in [Cyclesim.Config.t] when creating
      simulations for this to return (Some _). Writing to the [Bits.Mutable.t]
      will change the simulation internal node's value and affect the results of
      simulation.
  *)
  val lookup_reg : _ t -> Traced.internal_signal -> Reg.t option

  val lookup_reg_by_name : _ t -> string -> Reg.t option

  (** Similar to [lookup_data], but for memories. This is very useful
      for initializing memory contents without having to simulate the entire
      circuit.
  *)
  val lookup_mem : _ t -> Traced.internal_signal -> Memory.t option

  val lookup_mem_by_name : _ t -> string -> Memory.t option

  (** construct a simulator from a circuit *)
  val create
    :  ?implementation:[ `V1 | `V2 ]
    -> ?config:Config.t
    -> Circuit.t
    -> t_port_list

  module Combine_error = Cyclesim_combine.Combine_error

  (** Combine 2 simulators.  The inputs are set on the 1st simulator and copied to the
      2nd.  Outputs are checked and [on_error] is called if a difference is found.  By
      default, [on_error] raises.

      The simulators should have the same input and output port sets, unless
      [port_sets_may_differ] is [true], in which case only ports which exist on both
      simulators are checked. *)
  val combine
    :  ?port_sets_may_differ:bool (** Default is [false]. *)
    -> ?on_error:(Combine_error.t -> unit)
    -> ('i, 'o) t
    -> ('i, 'o) t
    -> ('i, 'o) t

  module With_interface (I : Interface.S) (O : Interface.S) : sig
    type nonrec t = (Bits.t ref I.t, Bits.t ref O.t) t [@@deriving sexp_of]

    (** Create a simulator using the provided [Create_fn].  The returned simulator ports
        are coerced to the input and output interface types. *)
    val create
      :  ?implementation:[ `V1 | `V2 ]
      -> ?config:Config.t
      -> ?circuit_config:Circuit.Config.t
      -> Circuit.With_interface(I)(O).create
      -> t

    (** Coerce simulator port types to use the provided input and output interfaces. *)
    val coerce : t_port_list -> t
  end

  module Private : sig
    include
      Cyclesim0.Private
        with type ('i, 'o) t := ('i, 'o) t
         and type port_list = Port_list.t
         and type t_port_list := t_port_list
         and type traced := Traced.t
         and type traced_io_port := Traced.io_port
         and type traced_internal_signal := Traced.internal_signal
         and type reg = Reg.t
         and type node = Node.t
         and type memory = Memory.t

    module Traced_nodes : module type of Cyclesim_compile.Traced_nodes
  end
end
