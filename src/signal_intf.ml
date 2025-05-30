open Base

module type Names = sig
  type t

  (** Returns the list of names and locations assigned to the signal. *)
  val names_and_locs : t -> Name_and_loc.t list

  (** Returns the list of names assigned to the signal. *)
  val names : t -> string list

  (** Set the given names on the signal. Wipes any names currently set. *)
  val set_names : t -> Name_and_loc.t list -> unit
end

module type Attributes = sig
  type t

  (** Add an attribute to node. *)
  val add_attribute : t -> Rtl_attribute.t -> t

  (** Returns attributes associated to the signal. *)
  val attributes : t -> Rtl_attribute.t list

  (** Set the format used to display the signal *)
  val ( --$ ) : t -> Wave_format.t -> t
end

module type Comments = sig
  type t

  (** Set the comment associated with the signal. This is currently only supported in
      Verilog. *)
  val set_comment : t -> string -> t

  (** Remove the comment associated with the signal. This is currently only supported in
      Verilog. *)
  val unset_comment : t -> t

  (** Returns comment associated with the signal. *)
  val comment : t -> string option
end

module type Ports = sig
  type t

  (** Creates an input. *)
  val input : string -> int -> t

  (** Creates an output. *)
  val output : string -> t -> t
end

module type Wires = sig
  type t

  (** Creates an unassigned wire. *)
  val wire : int -> t

  (** Creates an assigned wire. *)
  val wireof : t -> t

  (** Assigns to wire. *)
  val ( <-- ) : t -> t -> unit

  val assign : t -> t -> unit
end

module type Logic = sig
  type t

  (** Combinational logic API with constant propogation optimizations. *)
  include Comb.S with type t := t

  (** Combinational logic API without constant propogation optimizations. *)
  module Unoptimized : Comb.S with type t := t
end

module type Regs = sig
  type t

  module Reg_spec : Reg_spec.S with type signal := t

  type 'a with_register_spec =
    ?enable:t
    -> ?initialize_to:t
    -> ?reset_to:t
    -> ?clear:t
    -> ?clear_to:t
    -> Reg_spec.t
    -> 'a

  val reg : (t -> t) with_register_spec
  val reg_fb : (width:int -> f:(t -> t) -> t) with_register_spec

  (** Pipeline a signal [n] times with the given register specification. If set, a list of
      RTL attributes will also be applied to each register created. *)
  val pipeline : ?attributes:Rtl_attribute.t list -> (n:int -> t -> t) with_register_spec

  (** [Staged.unstage (prev spec ?enable d)] returns a function [prev n] which provides
      [d] registered [n] times (ie the value of [d] [n] cycles in the past). [n=0] means
      the current (combinational value).

      The internal registers are shared between calls. When called multiple times with a
      maximum value of [n] exactly [n] registers are created. *)
  val prev : (t -> (int -> t) Staged.t) with_register_spec
end

module type Memories = sig
  type t

  (** Synchronous write / asynchronous read memory. Supports an arbitrary number of
      [read_addresses] and [write_ports]. Placement of registers on the address or data
      output will allow synthesizers to infer RAMs from generated RTL (see also
      [Ram.create] which builds on this primitive to implement inferred memories).

      Any provided attributes are applied to the memory array itself.

      The memory may be initialized by providing [initialize_to].

      [enable_modelling_features] allows multibit enable and port scaling features. In
      both cases they will generate valid synthesizable RTL but backend tool inference is
      unlikely to work leading to sub-optimal designs.

      In both cases extra read/write ports are generated and the internal RAM may be
      narrowed to support the feature.

      For multibit enables the number of write enables must evenly divide the write data
      width or an exception will be raised.

      For port scaling the read/write port ratio must be a power of two. It is specified
      by making the read and write address widths different and scaling the write data
      width appropriately. The [size] and [initialize_to] parameters are specified
      relative to the write port with the widest data bus. *)
  val multiport_memory
    :  ?enable_modelling_features:bool
    -> ?verbose:bool
    -> ?name:string
    -> ?attributes:Rtl_attribute.t list
    -> ?initialize_to:Bits.t array
    -> int
    -> write_ports:t Write_port.t array
    -> read_addresses:t array
    -> t array

  (** A multi-read port asynchronous ROM built from a memory primitive. This can be used
      to map ROMs into RAM resources by registering the output. *)
  val rom : read_addresses:t array -> Bits.t array -> t array

  val memory : int -> write_port:t Write_port.t -> read_address:t -> t

  val ram_wbr
    :  ?name:string
    -> ?attributes:Rtl_attribute.t list
    -> write_port:t Write_port.t
    -> read_port:t Read_port.t
    -> int
    -> t

  val ram_rbw
    :  ?name:string
    -> ?attributes:Rtl_attribute.t list
    -> write_port:t Write_port.t
    -> read_port:t Read_port.t
    -> int
    -> t
end

module type S = sig
  (** Signal type for constructing logic designs. *)

  type t

  (** {1 Naming}

      One or more string names applied to a signal. These will be used during RTL code
      generation and simulation to aid design inspection. *)

  include Names with type t := t

  (** {1 Attributes}

      Attributes are attached to signals and written during RTL generation. They can be
      used to control downstream synthesis and place and route tools. They do not
      otherwise affect the Hardcaml circuit. *)

  include Attributes with type t := t

  (** {1 Comments}

      A comment can be associated with a signal, and this is sometimes useful to pass
      information to downstream tooling (specifically Verilator). *)

  include Comments with type t := t

  (** {1 Circuit Input and Output Ports}

      Specification of the module level input and output ports. Ports are represented in
      Hardcaml as wires with a single name. Note that Hardcaml will generally rewrite and
      legalize names depending on context but will not do so for ports. *)

  include Ports with type t := t

  (** {1 Wires}

      Wires are used in Hardcaml to conenct two signals together. They are first created
      then assigned to (internally they have a mutable reference to their driver). Wires
      are how we can create cycles which we require in order to build logic like, for
      example, a counter.

      It is entirely possible to create combinational loops using wires. Often this is not
      the intent and Hardcaml has functions to detect this - for example, Cyclesim will
      fail if there is such a loop. *)

  include Wires with type t := t

  (** {1 Combinational Logic}

      The main combinational logic API for creating things like adders, multiplexers etc.
      This API is the same as provided by [Bits.t].

      By default Hardcaml performs constant propogation over signals - operations with
      purely constant values (and some other simple cases such as [a &: vdd]) will be
      simplified. You can avoid this by using the operations from the [Unoptimized]
      module. *)

  include Logic with type t := t

  (** {1 Registers}

      Registers are configured using a [Reg_spec.t]. This encodes the clock, synchronous
      clear, and asychronous reset used. *)

  include Regs with type t := t

  (** {1 Memories}

      [multiport_memory] provides the low-level primitive from which Hardcaml memories are
      created. It provides a memory with an arbitrary number of read and write ports that
      is asychronously read.

      By default synthesizers will infer either LUT ram or register banks from this
      primitive.

      Limiting the number of read/write ports and placing a register on the read address
      or read output port will allow synthesizers to infer single or simple dual port RAM
      from RTL. [ram_rbw] and [ram_wbr] are examples of this. *)

  include Memories with type t := t

  (** Pretty printer. *)
  val pp : Formatter.t -> t -> unit

  (**/**)

  (* Apply a name to a signal using the hardcaml ppx [%hw] extension *)
  val __ppx_auto_name : ?loc:Stdlib.Lexing.position -> t -> string -> t
end

module type Signal = sig
  type t = Signal__type.t

  module Type = Signal__type
  include S with type t := Signal__type.t

  (** Returns the unique id of the signal. *)
  val uid : t -> Type.Uid.t
end
