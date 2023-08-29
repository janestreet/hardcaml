(** VCD (Verilog Change Dump) generation utilities, and a Cyclesim wrapper function.

    This deals with the gory details of the format, but external code is expected to
    manage the generation of data values and properly detect they are the same and
    should not be included.

    To generate a vcd;

    {v
      - build the variables and scopes, and assocaite data generators with variables
      - write_header
      - iteratate, while outputting changed values:
        - write_time
        - Var.write...
        - Var.write...
        - ...
   v}
*)

open Base
open Stdio

(** Timescales.

    Note the integer part must be 1, 10 or 100 according to the standard. Dont know why.
*)
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

      Not at all sure what they each do, and I suspect it's waveform viewer dependant.
  *)
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
      | Want
      | Wire
      | Wor
    [@@deriving sexp_of]

    val to_string : t -> string
  end

  type t [@@deriving sexp_of, compare, hash]

  val create : ?typ:Type.t -> name:string -> id:string -> width:int -> unit -> t
  val typ : t -> Type.t
  val name : t -> string
  val id : t -> string
  val width : t -> int
  val define : Out_channel.t -> t -> unit
  val write_string : Out_channel.t -> t -> string -> unit
  val write_bits : Out_channel.t -> t -> Bits.t -> unit
  val write_four_state_vector : Out_channel.t -> t -> Logic.Four_state_vector.t -> unit

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

(** Write a time in the data portion.  Should be followed by data values. *)
val write_time : Out_channel.t -> int -> unit

(** wrap a [Cyclesim] simulator to generate a vcd file.

    Flush the out channel! *)
val wrap : Out_channel.t -> ('i, 'o) Cyclesim.t -> ('i, 'o) Cyclesim.t
