(** Creation and manipulation of hardware circuits *)

open! Core0

(** circuit data structure *)
type t [@@deriving sexp_of]

(** Check if the ports specified in the interface match those defined in the circuit. *)
module Port_checks : sig
  type t =
    | Relaxed (** No checks *)
    | Port_sets (** Input and output port sets agree *)
    | Port_sets_and_widths
    (** Input and output port sets agree, and their widths are the same. *)
  [@@deriving sexp_of]
end

module Config : sig
  type t =
    { detect_combinational_loops : bool
    (** Check circuit for combinational loops (cyclic paths that do not pass through a
        register or memory). *)
    ; property_manager : Property_manager.t option
    ; trace_properties : Property_manager.Trace_properties.t
    (** Selects which of the [property_manager]'s properties are emitted (by type and
        group). *)
    ; port_checks : Port_checks.t
    (** Perform validation checks on inputs and outputs ([With_interface] only) *)
    ; add_phantom_inputs : bool
    (** Add inputs defined in an [Interface] but not used within the [Circuit]
        ([With_interface] only). *)
    }
  [@@deriving sexp_of]

  (** Perform combinational loop checks, strict port checks, and add phantom inputs. *)
  val default : t

  (** Default checks for simulations. Drop combinational loop checks (the simulation
      scheduler will do that anyway). Used by [Cyclesim.With_interface]. *)
  val default_for_simulations : t
end

(** create circuit data structure *)
val create_exn : ?config:Config.t -> name:string -> Signal.t list -> t

(** return circuit inputs *)
val inputs : t -> Signal.t list

(** return circuit outputs *)
val outputs : t -> Signal.t list

val signal_graph : t -> Signal_graph.t

(** return circuit name *)
val name : t -> string

(** returns the caller id of the circuit if one exists *)
val caller_id : t -> Caller_id.t option

(** Return the config the circuit was built with. *)
val config : t -> Config.t

(** Return identical circuit except for the name. *)
val with_name : t -> name:string -> t

(** is the signal an input to the circuit *)
val is_input : t -> Signal.t -> bool

(** is the signal an output of the circuit *)
val is_output : t -> Signal.t -> bool

(** For internal use. Add phantom input ports to the circuit when writing RTL. This can be
    necessary to ensure [Interface] based input specifications match those discovered when
    traversing the hardware design from its outputs. It is especially important when
    working with hierarchical designs. *)
val set_phantom_inputs : t -> (string * int) list -> t

val phantom_inputs : t -> (string * int) list

(** Construct a map of [uid]s to [Signal.t]s. *)
val signal_map : t -> Signal.t Signal.Type.Uid.Map.t

(** compare 2 circuits to see if they are the same *)
val structural_compare : ?check_names:bool -> t -> t -> bool

(** returns the list of instantiations in this circuit *)
val instantiations : t -> Signal.t Signal.Type.Inst.t list

(** {2 Properties} *)

val property_manager : t -> Property_manager.t option
val sva_properties : t -> (string, Signal.t Property.Sva.t) Hashtbl.t option
val sva_propositions : t -> Set.M(Signal.Type.Signal_compared_by_uid).t

(** {2 Sanitized and mangled RTL names for circuit components} *)
module Name_for_rtl : sig
  (** Rtl names for an internal signal. This will raise if the signal is not part of the
      circuit or is one of its inputs or outputs. *)
  val internal_signal_exn : t -> Signal.t -> Rtl_name.t list

  (** Rtl name for a instantiation. This will raise if the signal is not part of the
      circuit *)
  val instantiation_exn : t -> Signal.t Signal.Type.Inst.t -> Rtl_name.t

  (** Rtl name for a memory array and (in VHDL) array type. This will fail if the signal
      is not part of the circuit *)
  val multiport_mem_exn
    :  t
    -> Signal.t Signal.Type.Multiport_mem.t
    -> Rtl_name.t * Rtl_name.t

  (** Rtl name for a circuit port. This will fail if the signal is not an input or output
      port of the circuit *)
  val port_exn : t -> Signal.t -> Rtl_name.t

  (** Rtl name for a phantom port. This will fail if there is no phantom port with the
      given name and size *)
  val phantom_port_exn : t -> string * int -> Rtl_name.t

  (** Names of properties *)
  val property_name_exn : t -> string -> Rtl_name.t
end

module With_interface (I : Interface.S) (O : Interface.S) : sig
  type create = Interface.Create_fn(I)(O).t

  (** Create a circuit with [inputs] and [outputs] automatically defined and labelled
      according to the input ([I]) and output ([O]) interfaces. *)
  val create_exn
    :  ?config:Config.t
    -> ?input_attributes:Rtl_attribute.t list I.t
    -> ?output_attributes:Rtl_attribute.t list O.t
    -> name:string
    -> create
    -> t
end
