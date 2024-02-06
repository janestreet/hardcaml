(** The definition of the main Hardcaml Signal type.

    Exposes functions for constructing and scruntinizing a graph of signals. The function
    here are relevant for building transformations and generators based on a Hardcaml
    design.  The [Signal] module exposes the functions used to build a Hardcaml design.

    This module is not directly exposed - it should be used through [Signal.Type].

    Note the double underscore in the module name is deliberate in order for Ocaml to
    infer reasonable type names for signals.
*)

open Base

(** Uids unqiuely label every hardcaml node with an integer value. *)
module type Uid = sig
  type t [@@deriving compare, hash, sexp_of]

  include Comparator.S with type t := t
  include Equal.S with type t := t

  val zero : t
  val one : t
  val to_int : t -> int
  val to_string : t -> string
  val generator : unit -> [ `New of unit -> t ] * [ `Reset of unit -> unit ]
end

module type Uid_set = sig
  module Uid : Uid

  type t = Set.M(Uid).t [@@deriving sexp_of]

  val empty : t
end

module type Uid_map = sig
  module Uid : Uid

  type 'v t = 'v Map.M(Uid).t
end

module type Is_a = sig
  type t
  type signal_op

  (** is the emtpy signal? *)
  val is_empty : t -> bool

  (** is the signal a register? *)
  val is_reg : t -> bool

  (** is the signal a memory? *)
  val is_mem : t -> bool

  (** is the signal a memory read port? *)
  val is_mem_read_port : t -> bool

  (** is the signal an instantiation? *)
  val is_inst : t -> bool

  (** is the signal a constant? *)
  val is_const : t -> bool

  (** is the signal a part selection? *)
  val is_select : t -> bool

  (** is the signal a wire? *)
  val is_wire : t -> bool

  (** is the signal the given operator? *)
  val is_op2 : signal_op -> t -> bool

  (** is the signal concatenation? *)
  val is_cat : t -> bool

  (** is the signal a multiplexer? *)
  val is_mux : t -> bool

  (** is the signal a not> *)
  val is_not : t -> bool
end

module type Printable = sig
  type t
  type register

  (** [sexp_of_signal_recursive ~depth signal] converts a signal recursively to a sexp for
      up to [depth] levels.  If [show_uids] is false then signal identifiers will not be
      printed.  [max_list_length] controls how many [mux] and [concat] arguments
      (dependancies) are printed. *)
  val sexp_of_signal_recursive
    :  ?show_uids:bool (** default is [false] *)
    -> depth:int
    -> t
    -> Sexp.t

  val sexp_of_t : t -> Sexp.t
  val sexp_of_register : register -> Sexp.t
  val to_string : t -> string
end

(** Functions for operating on signal dependencies.

    All operations are implemented with [fold]. [fold], [iter] and [rev_map] are the most
    efficient as they only allocate whats strictly required. [map] and [to_list] allocate
    the resulting list twice.
*)
module type Deps = sig
  type t

  val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a
  val iter : t -> f:(t -> unit) -> unit

  (** Map over the deps of [t].  Mapping occurs in order, but the result is reversed. *)
  val rev_map : t -> f:(t -> 'a) -> 'a list

  (** Implemented by [rev_map] with a list reversal at the end *)
  val map : t -> f:(t -> 'a) -> 'a list

  (** Implemented with [map] *)
  val to_list : t -> t list
end

module type Type = sig
  module Uid : Uid

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
    | Signal_lt
  [@@deriving sexp_of, compare, equal, hash]

  type signal_metadata =
    { mutable names : string list
    ; mutable attributes : Rtl_attribute.t list
    ; mutable comment : string option
    ; mutable caller_id : Caller_id.t option
    ; mutable wave_format : Wave_format.t
    }

  (** internal structure for tracking signals *)
  type signal_id =
    { s_id : Uid.t
    ; s_width : int
    ; mutable s_metadata : signal_metadata option
    }

  (** main signal data type *)
  type t =
    | Empty
    | Const of
        { signal_id : signal_id
        ; constant : Bits.t
        }
    | Op2 of
        { signal_id : signal_id
        ; op : signal_op
        ; arg_a : t
        ; arg_b : t
        }
    | Mux of
        { signal_id : signal_id
        ; select : t
        ; cases : t list
        }
    | Cat of
        { signal_id : signal_id
        ; args : t list
        }
    | Not of
        { signal_id : signal_id
        ; arg : t
        }
    | Wire of
        { signal_id : signal_id
        ; driver : t ref
        }
    | Select of
        { signal_id : signal_id
        ; arg : t
        ; high : int
        ; low : int
        }
    | Reg of
        { signal_id : signal_id
        ; register : register
        ; d : t
        }
    | Multiport_mem of
        { signal_id : signal_id
        ; size : int
        ; write_ports : t Write_port.t array
        }
    | Mem_read_port of
        { signal_id : signal_id
        ; memory : t
        ; read_address : t
        }
    | Inst of
        { signal_id : signal_id
        ; extra_uid : Uid.t
        ; instantiation : instantiation
        }

  (** These types are used to define a particular type of register as per the following
      template, where each part is optional:

      {v
       always @(?edge clock, ?edge reset)
         if (reset == reset_level) d <= reset_value;
         else if (clear == clear_level) d <= clear_value;
         else if (enable) d <= ...;
     v} *)
  and register =
    { reg_clock : t (** clock *)
    ; reg_clock_edge : Edge.t (** active clock edge *)
    ; reg_reset : t (** asynchronous reset *)
    ; reg_reset_edge : Edge.t (** asynchronous reset edge *)
    ; reg_reset_value : t (** asychhronous reset value *)
    ; reg_clear : t (** synchronous clear *)
    ; reg_clear_level : Level.t (** synchronous clear level *)
    ; reg_clear_value : t (** sychhronous clear value *)
    ; reg_enable : t (** global system enable *)
    }

  and instantiation =
    { inst_name : string (** name of circuit *)
    ; inst_instance : string (** instantiation label *)
    ; inst_generics : Parameter.t list (** [Parameter.int ...] *)
    ; inst_inputs : (string * t) list (** name and input signal *)
    ; inst_outputs : (string * (int * int)) list
        (** name, width and low index of output *)
    ; inst_lib : string
    ; inst_arch : string
    }
end

module type Signal__type = sig
  module type Uid = Uid
  module type Type = Type

  include Type

  module type Uid_set = Uid_set
  module type Uid_map = Uid_map

  module Uid_set : Uid_set with module Uid := Uid
  module Uid_map : Uid_map with module Uid := Uid

  module type Deps = Deps with type t := t

  (** Construction of custom dependencies. *)
  module Make_deps (Fold : sig
    val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a
  end) : Deps

  (** Represents a signal's full set of dependencies. *)
  module Deps : Deps

  module type Printable = Printable with type t := t and type register := register
  module type Is_a = Is_a with type t := t and type signal_op := signal_op

  include Printable
  include Is_a

  (** returns the (private) signal_id.  For internal use only. *)
  val signal_id : t -> signal_id option

  (** Returns the unique id of the signal. *)
  val uid : t -> Uid.t

  (** Width in bits of [t]. *)
  val width : t -> int

  (** Returns the list of names assigned to the signal. *)
  val names : t -> string list

  val caller_id : t -> Caller_id.t option

  (** Return the (binary) string representing a constants value. *)
  val const_value : t -> Bits.t

  (** Return [Some driver] if [t] is a wire and the driver is not [Empty]. *)
  val wire_driver : t -> t option

  (** Perform a recursive structural comparison of two signals. *)
  val structural_compare
    :  ?check_names:bool
    -> ?check_deps:bool
    -> ?initial_deps:Uid_set.t
    -> t
    -> t
    -> Uid_set.t * bool

  (** Return true if [t] has at least one name set. *)
  val has_name : t -> bool

  (** Creates a new signal uid. *)
  val new_id : unit -> Uid.t

  (** Resets the signal identifiers. *)
  val reset_id : unit -> unit

  (** Constructs a signal_id type. *)
  val make_id : int -> signal_id

  (** Functions for working with metadata. *)

  val add_attribute : t -> Rtl_attribute.t -> unit
  val get_attributes : t -> Rtl_attribute.t list
  val set_attributes : t -> Rtl_attribute.t list -> unit
  val set_comment : t -> string -> unit
  val get_comment : t -> string option
  val unset_comment : t -> unit
  val add_name : t -> string -> unit
  val set_names : t -> string list -> unit
  val set_wave_format : t -> Wave_format.t -> unit
  val get_wave_format : t -> Wave_format.t
end
