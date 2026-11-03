(** VCD (Verilog Change Dump) generation utilities, and a Cyclesim wrapper function.

    This deals with the gory details of the format, but external code is expected to
    manage the generation of data values and properly detect they are the same and should
    not be included.

    To generate a vcd;

    {v
      - build the variables and scopes, and assocaite data generators with variables
      - write_header
      - iteratate, while outputting changed values:
        - write_time
        - Var.write...
        - Var.write...
        - ...
    v} *)

open! Core0
open Stdio

(** Timescales.

    Note the integer part must be 1, 10 or 100 according to the standard. Dont know why. *)
module Timescale : sig
  type t =
    | Fs of int
    | Ps of int
    | Ns of int
    | Us of int
    | Ms of int
    | S of int
  [@@deriving sexp_of]

  val to_string : t -> string
end

(** [Var] declarations within the vcd. They make a name to a unique (short) identifier
    used in the VCD data section. *)
module Var : sig
  (** Various types of [Var]s.

      Not at all sure what they each do, and I suspect it's waveform viewer dependant. *)
  module Type : sig
    type t =
      | Event
      | Integer
      | Parameter
      | Real
      | Reg
      | Supply0
      | Supply1
      | Time
      | Tri
      | Triand
      | Trior
      | Trireg
      | Tri0
      | Tri1
      | Wand
      | Wire
      | Wor
    [@@deriving sexp_of]

    val to_string : t -> string
  end

  type t [@@deriving sexp_of, compare, hash]

  val create
    :  ?typ:Type.t
    -> ?wave_format:Wave_format.t
    -> name:string
    -> id:string
    -> width:int
    -> unit
    -> t

  val typ : t -> Type.t
  val name : t -> string
  val id : t -> string
  val width : t -> int
  val define : Out_channel.t -> t -> unit
  val write_string : Out_channel.t -> t -> string -> unit
  val write_bits : Out_channel.t -> t -> Bits.t -> unit
  val write_all_x : Out_channel.t -> t -> unit

  (** Construct unique IDs for [Var]s. *)
  module Generator : sig
    type t

    val min_id_char : int
    val max_id_char : int
    val id_char_range : int

    (** Create a new generator *)
    val create : unit -> t

    (** Create a new ID. *)
    val next : t -> string
  end
end

(** Scopes define the hierarchical relationships of [Var.t]s *)
module Scope : sig
  (** Types of scope *)
  module Type : sig
    type t =
      | Begin
      | Fork
      | Function
      | Module
      | Task
    [@@deriving sexp_of]

    val to_string : t -> string
  end

  type t

  val create
    :  ?subscopes:t list
    -> ?typ:Type.t
    -> name:string
    -> vars:Var.t list
    -> unit
    -> t

  (** Create a scope, automatically inferring the module hierarchy by splitting module
      names on the specified [split_on] character (defaulting to the hardcaml convention
      of '$' as the split character). *)
  val create_auto_hierarchy
    :  ?typ:Type.t
    -> ?split_on:char
    -> name:string
    -> vars:Var.t list
    -> unit
    -> t

  val name : t -> string
  val typ : t -> Type.t

  (** Variables at this scope *)
  val vars : t -> Var.t list

  val subscopes : t -> t list

  (** All variables at this scope and in subscopes *)
  val all_vars : t -> Var.t list

  (** Write the scope to the vcd header *)
  val write : Out_channel.t -> t -> unit
end

(** VCD header configuration and timescale. *)
module Config : sig
  type t =
    { date : string
    ; version : string
    ; comment : string option
    ; timescale : Timescale.t
    }
  [@@deriving sexp_of]

  val default : t
  val write : Out_channel.t -> t -> unit
end

(** Write out the vcd header. *)
val write_header : Out_channel.t -> config:Config.t -> scopes:Scope.t list -> unit

(** Write a time in the data portion. Should be followed by data values. *)
val write_time : Out_channel.t -> int -> unit

(** wrap a [Cyclesim] simulator to generate a vcd file.

    Flush the out channel! *)
val wrap : Out_channel.t -> ('i, 'o) Cyclesim.t -> ('i, 'o) Cyclesim.t

(** Generate a VCD file from mutable event-based waveform data that has already been
    populated.

    The waves are walked in time order; multiple events at the same time share a single
    [#time] marker. Variables are split into [inputs] / [outputs] / [various] scopes based
    on {!Wave_data.Type.t}, matching the layout produced by [wrap]; empty scopes are
    omitted. Hierarchical names are split on [$] like {!Scope.create_auto_hierarchy}.

    Unlike [wrap], this does not synthesise a clock or reset signal — those are expected
    to already be present in [waves] when relevant.

    Flush the out channel! *)
val write_mutable_event_based
  :  ?config:Config.t
  -> Out_channel.t
  -> Wave_data_in_mutable_events.t Wave_data.Wave.t array
  -> unit

(** Like {!write_mutable_event_based}, for 4-state waveform data. Impure [Binary] values
    use ["01XZ"] notation; [Index] and [Map] values are written as all-X because those
    formats have no impure mapping. *)
val write_mutable_event_based_four_state
  :  ?config:Config.t
  -> Out_channel.t
  -> Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t array
  -> unit

(** VCD is a 4-state format, so this always returns 4-state wave data. Use
    {!Wave_data.to_two_state} for a lossy conversion to 2-state data.

    This round-trips VCDs produced by {!wrap}, {!write_mutable_event_based}, and
    {!write_mutable_event_based_four_state}. Top-level scopes ["inputs"], ["outputs"], and
    ["various"] map to [Input], [Output], and [Internal]; other names map to [Internal].
    Remaining scope paths are joined with [$], with ["-inputs"] and ["-outputs"] mapped
    back to ["i"] and ["o"]. Wave formats cannot be recovered and are always [Bit_or Hex],
    except for real- and string-valued signals (below).

    Several [$var]s may declare the same identifier, which external simulators emit when a
    signal is dumped under more than one name. Each becomes its own wave, and all of them
    share one wave data allocation (so they have equal
    [Wave_data_in_mutable_events_four_state.unique_id]s and receive every value change).

    Real-valued signals get a 64-bit wave holding the IEEE bits of each double, displayed
    with a [Custom] wave format that prints the float with [%g]. Note that [Custom]
    formats do not survive bin_io serialization.

    String-valued signals ([$var string], written for VHDL enum and string types) get a
    32-bit wave: each distinct string the signal takes is interned in first-seen order,
    the wave stores indexes into that table, and the wave format is [Index] of the table
    (which does survive bin_io serialization).

    Every value change is recorded as an event, including changes in [$dumpvars],
    [$dumpall], [$dumpon], and [$dumpoff]. Hardcaml writers emit an initial all-X
    [$dumpvars] block, so each signal is X until its first driven value.

    @raise Error
      if the kind of a value change (bit vector, real, or string) does not match its
      signal's declaration. *)
val read_mutable_event_based_four_state
  :  Hardcaml_vcd.t
  -> Wave_data_in_mutable_events_four_state.t Wave_data.Wave.t array
