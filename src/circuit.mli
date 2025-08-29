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
    ; normalize_uids : bool
    (** Renumber the [Uid]s of all signals in the circuit starting at one.

        Uid normalization ensures that circuits will print the same (as sexps or rtl)
        regardless of the environment in which they are constructed (in particular with
        regard to the global uid generator). *)
    ; assertions : Assertion_manager.t option
    ; port_checks : Port_checks.t
    (** Perform validation checks on inputs and outputs ([With_interface] only) *)
    ; add_phantom_inputs : bool
    (** Add inputs defined in an [Interface] but not used within the [Circuit]
        ([With_interface] only). *)
    ; modify_outputs : Signal.t list -> Signal.t list
    (** Map over circuit outputs just before constructing the circuit. *)
    }
  [@@deriving sexp_of]

  (** Perform combinational loop checks, uid normalization, strict port checks, and add
      phantom inputs. *)
  val default : t

  (** Deftault checks for simulations. Drop combinational loop checks (the simulation
      scheduler will do that anyway) and uid normalization. Used by
      [Cyclesim.With_interface]. *)
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
val assertions : t -> Signal.t Map.M(String).t

(** Construct a map of [uid]s to [Signal.t]s. *)
val signal_map : t -> Signal.t Signal.Type.Uid_map.t

(** compare 2 circuits to see if they are the same *)
val structural_compare : ?check_names:bool -> t -> t -> bool

(** returns the list of instantiations in this circuit *)
val instantiations : t -> Signal.t Signal.Type.Inst.Instantiation.t list

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
