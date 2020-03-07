(** Creation and manipulation of hardware circuits *)

open! Import

(** circuit data structure *)
type t [@@deriving sexp_of]

(** [with_create_options] specifies the optional arguments that can be supplied to
    [create_exn].

    [detect_combinational_loops] determines whether [create_exn] ensures that there is no
    path from a signal to itself that does not pass through a register or memory.

    [normalize_uids] determines whether [create_exn] renumbers the uids of all signals in
    the circuit starting at one.  Uid normalization ensures that circuits will print the
    same (as sexps or rtl) regardless of the environment in which they are constructed (in
    particular with regard to the global uid generator). *)
type 'a with_create_options =
  ?detect_combinational_loops:bool (** default is [true] *)
  -> ?normalize_uids:bool (** default is [true] *)
  -> 'a

(** create circuit data structure  *)
val create_exn : (name:string -> Signal.t list -> t) with_create_options

(** return circuit inputs *)
val inputs : t -> Signal.t list

(** return circuit outputs *)
val outputs : t -> Signal.t list

val signal_graph : t -> Signal_graph.t

(** return circuit name *)
val name : t -> string

(** Return identical circuit except for the name. *)
val with_name : t -> name:string -> t

(** is the signal an input to the circuit *)
val is_input : t -> Signal.t -> bool

(** is the signal an output of the circuit *)
val is_output : t -> Signal.t -> bool

val find_signal_exn : t -> Signal.Uid.t -> Signal.t

(** For internal use.  Add phantom input ports to the circuit when writing RTL.  This
    can be necessary to ensure [Interface] based input specifications match those
    discovered when traversing the hardware design from its outputs.  It is especially
    important when working with hierarchical designs. *)
val set_phantom_inputs : t -> (string * int) list -> t

val phantom_inputs : t -> (string * int) list

(** Map of [uid]s to [Signal.t]s. *)
module Signal_map : sig
  type t = Signal.t Signal.Uid_map.t [@@deriving sexp_of]
end

(** Get map of [uid]s to [Signal.t]s. *)
val signal_map : t -> Signal_map.t

(** Compute and return a [Fan_out_map.t].  The computation is lazy and only performed the
    first time [fan_out_map] is called. *)
val fan_out_map : t -> Signal.Uid_set.t Signal.Uid_map.t

(** Compute and return a [Fan_in_map.t].  The computation is lazy and only performed the
    first time [fan_in_map] is called. *)
val fan_in_map : t -> Signal.Uid_set.t Signal.Uid_map.t

(** compare 2 circuits to see if they are the same *)
val structural_compare : ?check_names:bool -> t -> t -> bool

(** [Create_options] is a record with one field for each [with_create_options] argument.
    It allows one to define a function of type [_ with_create_options] that takes the same
    optional arguments as [create_exn] and to pass those options on to [create_exn],
    without every having to directly refer to any of the arguments.  This makes wrapper
    code robust to changes in what the optional arguments are.  Here is the usage idiom:

    {[
      module M : sig
        val f : (...) Circuit.with_create_options
      end = struct
        let f =
          Circuit.with_create_options (fun create_options ->
            ...
            call_with_create_options Circuit.create_exn create_options ~name signals;
            ...)
      end
    ]} *)
module Create_options : sig
  type t [@@deriving sexp_of]
end

val with_create_options : (Create_options.t -> 'a) -> 'a with_create_options
val call_with_create_options : 'a with_create_options -> Create_options.t -> 'a

(** Check if the ports specified in the interface match those defined in the circuit. *)
module Port_checks : sig
  type t =
    | Relaxed (** No checks *)
    | Port_sets (** Input and output port sets agree *)
    | Port_sets_and_widths
    (** Input and output port sets agree, and their widths are the same. *)
end

module With_interface (I : Interface.S) (O : Interface.S) : sig
  type create = Signal.t Interface.Create_fn(I)(O).t

  (** Create a circuit with [inputs] and [outputs] automatically defined and labelled
      according to the input ([I]) and output ([O]) interfaces. *)
  val create_exn
    : (?port_checks:Port_checks.t (** Default is [Relaxed]. *)
       -> ?add_phantom_inputs:bool (** Default is [true]. *)
       -> name:string
       -> create
       -> t)
        with_create_options
end
