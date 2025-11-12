(** The definition of the main Hardcaml Signal type.

    Exposes functions for constructing and scruntinizing a graph of signals. The function
    here are relevant for building transformations and generators based on a Hardcaml
    design. The [Signal] module exposes the functions used to build a Hardcaml design.

    This module is not directly exposed - it should be used through [Signal.Type].

    Note the double underscore in the module name is deliberate in order for Ocaml to
    infer reasonable type names for signals. *)

open! Core0

(** Uids unqiuely label every hardcaml node with an integer value. *)
module type Uid = Uid_builder.S

module type Uid_set = sig
  module Uid : Uid

  type t = Set.M(Uid).t [@@deriving sexp_of]

  val empty : t
end

module type Uid_map = sig
  module Uid : Uid

  type 'v t = 'v Map.M(Uid).t
end

(** Information attached to the signals. *)
module type With_info = sig
  type t

  (** Information tracked by the signal. *)
  type info

  (** Get the information attached to a signal. *)
  val info : t -> info

  (** Set the information attached to a signal. *)
  val set_info : t -> info:info -> t
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

  (** is the signal a cases selection? *)
  val is_cases : t -> bool

  (** is the signal a not> *)
  val is_not : t -> bool
end

module type Printable = sig
  type t
  type register
  type reg_spec

  (** [sexp_of_signal_recursive ~depth signal] converts a signal recursively to a sexp for
      up to [depth] levels. If [show_uids] is false then signal identifiers will not be
      printed. [show_locs] includes source code locations. *)
  val sexp_of_signal_recursive
    :  ?show_uids:bool (** default is [false] *)
    -> ?show_locs:bool (** default is [false] *)
    -> depth:int
    -> t
    -> Sexp.t

  val sexp_of_t : t -> Sexp.t
  val sexp_of_register : register -> Sexp.t
  val sexp_of_reg_spec : reg_spec -> Sexp.t
  val to_string : t -> string
end

(** Functions for operating on signal dependencies.

    All operations are implemented with [fold]. [fold], [iter] and [rev_map] are the most
    efficient as they only allocate whats strictly required. [map] and [to_list] allocate
    the resulting list twice. *)
module type Deps = sig
  type t

  val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a
  val iter : t -> f:(t -> unit) -> unit

  (** Map over the deps of [t]. Mapping occurs in order, but the result is reversed. *)
  val rev_map : t -> f:(t -> 'a) -> 'a list

  (** Implemented by [rev_map] with a list reversal at the end *)
  val map : t -> f:(t -> 'a) -> 'a list

  (** Implemented with [map] *)
  val to_list : t -> t list
end

module type Type = sig
  module Uid : Uid

  (** simple operators *)
  module Op : sig
    type t =
      | Add
      | Sub
      | Mulu
      | Muls
      | And
      | Or
      | Xor
      | Eq
      | Lt
    [@@deriving bin_io, sexp_of, compare ~localize, equal ~localize, hash]
  end

  module Metadata : sig
    type t =
      { mutable names_and_locs : Name_and_loc.t list
      ; mutable attributes : Rtl_attribute.t list
      ; mutable comment : string option
      ; caller_id : Caller_id.t option
      ; mutable wave_format : Wave_format.t
      ; mutable coverage : Coverage_metadata.t option
      }
    [@@deriving bin_io, sexp_of]
  end

  (** internal structure for tracking signals *)
  module Info : sig
    type t =
      { uid : Uid.t
      ; width : int
      ; mutable metadata : Metadata.t option
      }
    [@@deriving bin_io, sexp_of]
  end

  module Const : sig
    type t =
      { info : Info.t
      ; constant : Bits.t
      }
    [@@deriving bin_io, sexp_of]
  end

  module Op2 : sig
    type 'signal t =
      { info : Info.t
      ; op : Op.t
      ; arg_a : 'signal
      ; arg_b : 'signal
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Mux : sig
    type 'signal t =
      { info : Info.t
      ; select : 'signal
      ; cases : 'signal list
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Cases : sig
    type 'signal t =
      { info : Info.t
      ; select : 'signal
      ; cases : ('signal * 'signal) list
      ; default : 'signal
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Cat : sig
    type 'signal t =
      { info : Info.t
      ; args : 'signal list
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Not : sig
    type 'signal t =
      { info : Info.t
      ; arg : 'signal
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Wire : sig
    type 'signal t =
      { info : Info.t
      ; mutable driver : 'signal option
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Select : sig
    type 'signal t =
      { info : Info.t
      ; arg : 'signal
      ; high : int
      ; low : int
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Reg : sig
    module Clock_spec : sig
      type 'a t =
        { clock : 'a
        ; clock_edge : Edge.t
        }
      [@@deriving bin_io, sexp_of]

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end

    module Reset_spec : sig
      type 'a t =
        { reset : 'a
        ; reset_edge : Edge.t
        ; reset_to : 'a
        }
      [@@deriving bin_io, sexp_of]

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end

    module Clear_spec : sig
      type 'a t =
        { clear : 'a
        ; clear_to : 'a
        }
      [@@deriving bin_io, sexp_of]

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end

    module Register : sig
      type 'a t =
        { clock : 'a Clock_spec.t
        ; reset : 'a Reset_spec.t option
        ; clear : 'a Clear_spec.t option
        ; enable : 'a option
        ; initialize_to : Bits.t option
        }
      [@@deriving bin_io, sexp_of]

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end

    type 'signal t =
      { info : Info.t
      ; register : 'signal Register.t
      ; d : 'signal
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Multiport_mem : sig
    type 'signal t =
      { info : Info.t
      ; size : int
      ; write_ports : 'signal Write_port.t array
      ; initialize_to : Bits.t array option
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Mem_read_port : sig
    type 'signal t =
      { info : Info.t
      ; memory : 'signal
      ; read_address : 'signal
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Inst : sig
    module Input : sig
      type 'signal t =
        { name : string
        ; input_signal : 'signal
        }
      [@@deriving bin_io, sexp_of]

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end

    module Output : sig
      type t =
        { name : string
        ; output_width : int
        ; output_low_index : int
        }
      [@@deriving equal]
    end

    module Vhdl_instance : sig
      type t =
        { library_name : string
        ; architecture_name : string
        }
      [@@deriving bin_io, sexp_of]
    end

    module Instantiation : sig
      type 'signal t =
        { circuit_name : string
        ; instance_label : string
        ; parameters : Parameter.t list
        ; inputs : 'signal Input.t list
        ; outputs : Output.t list
        ; vhdl_instance : Vhdl_instance.t
        }
      [@@deriving bin_io, sexp_of]

      val map : 'a t -> f:('a -> 'b) -> 'b t
    end

    type 'signal t =
      { info : Info.t
      ; instantiation : 'signal Instantiation.t
      }
    [@@deriving bin_io, sexp_of]

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  (** main signal data type *)
  type t =
    | Empty
    | Const of Const.t
    | Op2 of t Op2.t
    | Mux of t Mux.t
    | Cases of t Cases.t
    | Cat of t Cat.t
    | Not of t Not.t
    | Wire of t Wire.t
    | Select of t Select.t
    | Reg of t Reg.t
    | Multiport_mem of t Multiport_mem.t
    | Mem_read_port of t Mem_read_port.t
    | Inst of t Inst.t

  (** These types are used to define a particular type of register as per the following
      template, where each part is optional:

      {v
       reg [7:0] d = initialize_to;

       always @(?edge clock, ?edge reset)
         if (reset == reset_level) d <= reset_to;
         else if (clear) d <= clear_to;
         else if (enable) d <= ...;
      v} *)

  and reg_spec =
    { clock : t
    ; clock_edge : Edge.t
    ; reset : t option
    ; reset_edge : Edge.t
    ; clear : t option
    }
end

module type Register = sig
  type signal
  type reg_spec
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val of_reg_spec
    :  reg_spec
    -> enable:signal option
    -> initialize_to:Bits.t option
    -> reset_to:signal option
    -> clear_to:signal option
    -> clear:signal option
    -> signal
    -> signal t
end

module type Signal__type = sig
  module type Uid = Uid
  module type Type = Type

  include Type

  module type Uid_set = Uid_set
  module type Uid_map = Uid_map

  module Uid_set : Uid_set with module Uid := Uid
  module Uid_map : Uid_map with module Uid := Uid

  module type With_info = With_info
  module type Deps = Deps with type t := t

  (** Default implementation of [With_info] that uses unit types. *)
  module Make_default_info (S : sig
      type t
    end) : With_info with type t := S.t and type info = unit

  (** Construction of custom dependencies. *)
  module Make_deps (Fold : sig
      val fold : t -> init:'a -> f:('a -> t -> 'a) -> 'a
    end) : Deps

  (** Represents a signal's full set of dependencies. *)
  module Deps : Deps

  module Register :
    Register
    with type 'a t = 'a Reg.Register.t
     and type signal := t
     and type reg_spec := reg_spec

  module type Printable =
    Printable
    with type t := t
     and type register := t Reg.Register.t
     and type reg_spec := reg_spec

  module type Is_a = Is_a with type t := t and type signal_op := Op.t

  include Printable
  include Is_a

  (** returns the (private) signal info. For internal use only. *)
  val info : t -> Info.t option

  (** Returns the unique id of the signal. *)
  val%template uid : t @ m -> Uid.t @ m
  [@@mode m = (local, global)]

  (** Width in bits of [t]. *)
  val width : t -> int

  (** Returns the list of names and source locations assigned to the signal. *)
  val names_and_locs : t -> Name_and_loc.t list

  (** Returns the list of names assigned to the signal. *)
  val names : t -> string list

  (** Returns the caller id of the signal if one exists. *)
  val caller_id : t -> Caller_id.t option

  (** Returns coverage metadata for the signal if it exists. *)
  val coverage_metadata : t -> Coverage_metadata.t option

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

  (** The default wave format for new signals. *)
  val default_wave_format : Wave_format.t

  (** Creates a new signal uid. *)
  val new_id : unit -> Uid.t

  (** Constructs a [Info.t] type with given width. Allows zero width which can be used for
      instantiations with no outputs. *)
  val make_id_allow_zero_width : int -> Info.t

  (** Constructs a [Info.t] type with given [width>0]. *)
  val make_id : int -> Info.t

  (** Create a constant. *)
  val of_bits : Bits.t -> t

  (** Returns true iff [t] is a constant, has width [1] and equals the value [1]. Does not
      raise. *)
  val is_vdd : t -> bool

  (** Returns true iff [t] is a constant, has width [1] and equals the value [0]. Does not
      raise. *)
  val is_gnd : t -> bool

  (** Signal is a register or a memory with an [initialize_to] value specified. *)
  val has_initializer : t -> bool

  (** Functions for working with metadata. *)

  val add_attribute : t -> Rtl_attribute.t -> unit
  val get_attributes : t -> Rtl_attribute.t list
  val set_attributes : t -> Rtl_attribute.t list -> unit
  val set_comment : t -> string -> unit
  val get_comment : t -> string option
  val unset_comment : t -> unit
  val add_name : t -> Name_and_loc.t -> unit
  val set_names : t -> Name_and_loc.t list -> unit
  val set_wave_format : t -> Wave_format.t -> unit
  val get_wave_format : t -> Wave_format.t

  val update_coverage_metadata
    :  t
    -> f:(Coverage_metadata.t option -> Coverage_metadata.t)
    -> unit

  (** This function creates a copy of the signal with [f] applied to the signal's info (if
      applicable). *)
  val map_info : t -> f:(Info.t -> Info.t) -> t

  (** This function creates a copy of the signal with [f] applied to each of the signal's
      dependants. *)
  val map_dependant : t -> f:(t -> t) -> t
end
