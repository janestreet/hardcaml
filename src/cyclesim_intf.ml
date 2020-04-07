open! Import

module type Cyclesim = sig
  module Port_list : sig
    type t = (string * Bits.t ref) list [@@deriving sexp_of]
  end

  (** base type of the cycle based simulators *)
  type ('i, 'o) t

  type t_port_list = (Port_list.t, Port_list.t) t

  (** Specialised signal dependencies that define a graph that breaks cycles through
      sequential elements.  This is done by removing the input edges of registers and
      memories (excluding the read address, since hardcaml memories are read
      asynchronously).

      Instantiations do not allow cycles from output to input ports, which is a valid
      assumption for the simulator, but not in general.

      Note that all signals in the graph cannot be reached from just the outputs of a
      circuit using these dependencies.  The (discarded) inputs to all registers and
      memories must also be included. *)
  val scheduling_deps : Signal.t -> Signal.t list

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

  (** get list of internal nodes *)
  val internal_ports : _ t -> Port_list.t

  val lookup_signal : _ t -> Signal.Uid.t -> Bits.t ref
  val lookup_reg : _ t -> Signal.Uid.t -> Bits.t ref

  type 'a with_create_options =
    ?is_internal_port:(Signal.t -> bool)
    -> ?combinational_ops_database:Combinational_ops_database.t
    -> 'a

  (** construct a simulator from a circuit *)
  val create : (Circuit.t -> t_port_list) with_create_options

  module Combine_error : sig
    type t =
      { cycle_no : int
      ; clock_edge : Side.t
      ; port_name : string
      ; value0 : Bits.t
      ; value1 : Bits.t
      }
    [@@deriving sexp_of]
  end

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
      : (?port_checks:Circuit.Port_checks.t
         -> ?add_phantom_inputs:bool
         -> Circuit.With_interface(I)(O).create
         -> t)
          with_create_options
          Circuit.with_create_options

    (** Coerce simulator port types to use the provided input and output interfaces. *)
    val coerce : t_port_list -> t
  end

  module Private : sig
    type task = unit -> unit

    val create
      :  in_ports:Port_list.t
      -> out_ports_before_clock_edge:Port_list.t
      -> out_ports_after_clock_edge:Port_list.t
      -> internal_ports:Port_list.t
      -> reset:task
      -> cycle_check:task
      -> cycle_before_clock_edge:task
      -> cycle_at_clock_edge:task
      -> cycle_after_clock_edge:task
      -> lookup_signal:(Signal.Uid.t -> Bits.t ref)
      -> lookup_reg:(Signal.Uid.t -> Bits.t ref)
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
      :  (Port_list.t, Port_list.t) t
      -> to_input:(Port_list.t -> 'i)
      -> to_output:(Port_list.t -> 'o)
      -> ('i, 'o) t
  end
end
