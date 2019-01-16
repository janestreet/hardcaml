(** Hardware design datatype suitable for simulation and netlist generation *)

open! Import

module type Signal = sig

  (** Signal data type and low-level functions *)

  (** simple operators *)
  type signal_op =
    | Signal_add
    | Signal_sub
    | Signal_mulu
    | Signal_muls
    | Signal_and
    | Signal_or
    | Signal_xor
    | Signal_eq
    | Signal_not
    | Signal_lt
    | Signal_cat
    | Signal_mux

  module Uid : sig
    type t = int64 [@@deriving compare, hash, sexp_of]
    include Comparator.S with type t := t
    include Equal.S      with type t := t
  end

  module Uid_map : Map.S with module Key := Uid

  module Uid_set : sig
    type t = Set.M(Uid).t [@@deriving sexp_of]

    val empty : t
  end

  (** internal structure for tracking signals *)
  type signal_id =
    { s_id            : Uid.t
    ; mutable s_names : string list
    ; s_width         : int
    ; (** Making this mutable turns hardcaml from pretty functional to pretty imperative.
          however, if used carefully and only with the library, we can provide a
          potentially easier way of changing the graph structure in some cases *)
      mutable s_deps  : t list
    ; caller_id       : Caller_id.t option }

  (** main signal data type *)
  and t =
    | Empty
    | Const of signal_id * Bits.t
    | Op of signal_id * signal_op
    | Wire of signal_id * t ref
    | Select of signal_id * int * int
    | Reg of signal_id * register
    | Mem of signal_id * Uid.t * register * memory
    | Multiport_mem of signal_id * int * write_port array
    | Mem_read_port of signal_id * t * t
    | Inst of signal_id * Uid.t * instantiation

  and write_port =
    { write_clock : t
    ; write_address : t
    ; write_enable : t
    ; write_data : t }

  and read_port =
    { read_clock : t
    ; read_address : t
    ; read_enable : t }

  (** These types are used to define a particular type of register as per the following
      template, where each part is optional:

      {v
       always @(?edge clock, ?edge reset)
         if (reset == reset_level) d <= reset_value;
         else if (clear == clear_level) d <= clear_value;
         else if (enable) d <= ...;
     v} *)
  and register =
    { reg_clock       : t       (** clock *)
    ; reg_clock_edge  : Edge.t  (** active clock edge *)
    ; reg_reset       : t       (** asynchronous reset *)
    ; reg_reset_edge  : Edge.t  (** asynchronous reset edge *)
    ; reg_reset_value : t       (** asychhronous reset value *)
    ; reg_clear       : t       (** synchronous clear *)
    ; reg_clear_level : Level.t (** synchronous clear level *)
    ; reg_clear_value : t       (** sychhronous clear value *)
    ;
      reg_enable      : t       (** global system enable *) }

  and memory =
    { mem_size          : int
    ; mem_read_address  : t
    ; mem_write_address : t }

  and instantiation =
    { inst_name     : string                      (** name of circuit *)
    ; inst_instance : string                      (** instantiation label *)
    ; inst_generics : Parameter.t list            (** [Parameter.int ...] *)
    ; inst_inputs   : (string * t) list           (** name and input signal *)
    ; inst_outputs  : (string * (int * int)) list (** name, width and low index of output *)
    ; inst_lib      : string
    ; inst_arch     : string }

  type signal = t

  (** returns the unique id of the signal *)
  val uid : t -> Uid.t

  (** returns the signal's dependencies *)
  val deps : t -> t list

  (** returns the list of names assigned to the signal *)
  val names : t -> string list

  val has_name : t -> bool

  (** is the signal a register? *)
  val is_reg : t -> bool

  (** is the signal a memory, or multiport memory? *)
  val is_mem : t -> bool

  (** is the signal a multiport memory? *)
  val is_multiport_mem : t -> bool

  (** is the signal an instantiation? *)
  val is_inst : t -> bool

  (** is the signal a constant? *)
  val is_const : t -> bool

  (** is the signal a part selection? *)
  val is_select : t -> bool

  (** is the signal a wire? *)
  val is_wire : t -> bool

  (** is the signal the given operator? *)
  val is_op : signal_op -> t -> bool

  (** return the (binary) string representing a constants value *)
  val const_value : t -> Bits.t

  (** creates a new signal uid *)
  val new_id : unit -> Uid.t

  (** resets the signal identifiers *)
  val reset_id : unit -> unit

  (** constructs a signal_id type *)
  val make_id : int -> t list -> signal_id

  (** perform a recursive structural comparison of two signals *)
  val structural_compare
    :  ?check_names:bool
    -> ?check_deps:bool
    -> t
    -> t
    -> bool

  (** [sexp_of_signal_recursive ~depth signal] converts a signal recursively to a sexp for
      up to [depth] levels.  If [show_uids] is false then signal identifiers will not be
      printed.  [max_list_length] controls how many [mux] and [concat] arguments
      (dependancies) are printed. *)
  val sexp_of_signal_recursive
    :  ?show_uids:bool      (** default is [false] *)
    -> depth:int
    -> t
    -> Sexp.t

  module Const_prop : sig
    module Comb : Comb.S with type t := t
  end

  (** Combinatorial signal API *)
  include Comb.S with type t := t

  (** creates an unassigned wire *)
  val wire : int -> t

  (** creates an assigned wire *)
  val wireof : t -> t

  (** assigns to wire *)
  val ( <== ) : t -> t -> unit
  val assign : t -> t -> unit

  (** creates an input *)
  val input : string -> int -> t

  (** creates an output *)
  val output : string -> t -> t

  (** [Reg_spec_] is a register specification.  It is named [Reg_spec_] rather than
      [Reg_spec] so that people consistently use the name [Hardcaml.Reg_spec] rather
      than [Hardcaml.Signal.Reg_spec_]. *)
  module Reg_spec_ : sig
    type t = register [@@deriving sexp_of]

    val create
      :  ?clear : signal
      -> ?reset : signal
      -> unit
      -> clock : signal
      -> t

    val override
      :  ?clock         : signal
      -> ?clock_edge    : Edge.t
      -> ?reset         : signal
      -> ?reset_edge    : Edge.t
      -> ?reset_to      : signal
      -> ?clear         : signal
      -> ?clear_level   : Level.t
      -> ?clear_to      : signal
      -> ?global_enable : signal
      -> t
      -> t

    val clock : t -> signal
    val clear : t -> signal
    val reset : t -> signal
  end

  val reg
    :  Reg_spec_.t
    -> enable:t
    -> t
    -> t

  val reg_fb
    :  Reg_spec_.t
    -> enable:t
    -> w:int
    -> (t -> t)
    -> t

  val pipeline
    :  Reg_spec_.t
    -> n:int
    -> enable:t
    -> t
    -> t

  val memory
    :  int
    -> write_port:write_port
    -> read_address:t
    -> t

  val ram_wbr
    :  int
    -> write_port:write_port
    -> read_port:read_port
    -> t

  val ram_rbw
    :  int
    -> write_port:write_port
    -> read_port:read_port
    -> t

  val multiport_memory
    :  int
    -> write_ports : write_port array
    -> read_addresses : t array
    -> t array

  (** Pretty printer. *)
  val pp : Formatter.t -> t -> unit

end
