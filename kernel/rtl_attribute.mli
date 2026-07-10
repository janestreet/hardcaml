open! Core0

(** RTL attribute specification. Only relevant to downstream tooling. *)

(** Specification of attributes which may be attached to various objects within a RTL
    design. Such attributes are used to provide implementation hints to down stream CAD
    tools and do not affect any functionality within Hardcaml. *)
type t [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp_of]

include Comparator.S with type t := t

(** Attribute value types. *)
module Value : sig
  type t =
    | Int of int
    | String of string
    | Bool of bool
  [@@deriving bin_io, compare ~localize, equal ~localize, sexp_of]
end

module Applies_to : sig
  type t =
    | Non_wires
    | Regs
    | Memories
    | Instantiations
  [@@deriving bin_io, compare ~localize, equal ~localize, sexp_of]
end

(** Create a new attribute. [applies_to], if specified, constrains what type of signal the
    attribute may be applied to. *)
val create : ?applies_to:Applies_to.t list -> ?value:Value.t -> string -> t

(** Returns the attribute name *)
val name : t -> string

(** Returns the attribute value, if any. *)
val value : t -> Value.t option

(** Signal types that the attribute may be attached to. *)
val applies_to : t -> Applies_to.t list

(** A collection of common Xilinx Vivado attributes. *)
module Vivado : sig
  (** Inform Vivado that a registers data input is asychronous to it's clock. *)
  val async_reg : bool -> t

  (** This attribute should be applied to registers on both sides of an SLR crossing, and
      instructs Vivado to place the register in a location optimal for connecting to the
      Super-long-lines (SLLs), giving better timing. This uses Laguna registers for
      Ultrascale architectures, and close-by slice registers for Versal. *)
  val user_sll_reg : bool -> t

  (** Instruct the synthesizer and place & route tools to keep the node. Cannot be applied
      to a port. *)
  val dont_touch : bool -> t

  (** Setting keep_hierarchy to yes will prevent Vivado from optimizating across module
      boundaries. Can only be applied to modules or instances. *)
  val keep_hierarchy : bool -> t

  (** Setting USE_DSP to yes can force an operation to utilize the DSP. *)
  val use_dsp : bool -> t

  (** Select encoding of finite state machine. Apply to state register. *)
  val fsm_encoding : [ `auto | `gray | `johnson | `none | `one_hot | `sequential ] -> t

  (** Export net for debugging with chipscope. *)
  val mark_debug : bool -> t

  (** Similar to dont_touch. *)
  val keep : bool -> t

  (** io_buffer_type Vivado attribute. Applied to top level ports and instructs Vivado not
      to infer the buffer type, instead using the attribute below. *)
  val io_buffer_type : [ `IBUF | `OBUF | `None ] -> t

  (** For registers, set the maximun fanout for that net. Suggests to Vivado to use
      register replication if fanout above [n]. *)
  val max_fanout : int -> t

  (** EXTRACT_ENABLE controls whether registers infer enables. Typically, Vivado will
      extract or not extract enables based on heuristics that typically benefit the most
      amount of designs. In cases where Vivado is not behaving in a desired way, this
      attribute overrides the default behavior of the tool.

      If there is an undesired enable going to the CE pin of the flip-flop, this attribute
      can force it to the D input logic. Conversely, if the tool is not inferring an
      enable that is specified in the RTL, this attribute can tell the tool to move that
      enable to the CE pin of the flip-flop. *)
  val extract_enable : bool -> t

  (** See [extract_enable] *)
  val extract_reset : bool -> t

  (** CRITICAL_SIG_OPT is used to optimize sequential loops by restructuring logic in the
      feedback path, so that timing-critical signals travel through the smallest number of
      logic levels. The attribute should be placed on sequential objects such as
      registers, that drive critical paths to their own inputs.

      The optimization improves critical path timing, but at the expense of increased
      logic utilization as it involves Shannon decomposition. You must mark the sequential
      elements (registers) with a loop which have reasonable logic levels. It can cause
      resource overhead due to logic replication. *)
  val critical_sig_opt : bool -> t

  val cascade_height : int -> t

  (** Instructs the tool to move a register forward through [int] levels of logic closer
      to the driven sequential elements. *)
  val retiming_forward : int -> t

  (** Instructs the tool to move a register backwards through [int] levels of logic closer
      to sequential driving elements. *)
  val retiming_backward : int -> t

  module Ram_style : sig
    val block : t
    val distributed : t
    val registers : t
    val ultra : t
  end

  (** SRL_STYLE instructs the synthesis tool on how to infer SRLs that are found in the
      design. Accepted values are

      - register: The tool does not infer an SRL, but instead only uses registers.
      - srl: The tool infers an SRL without any registers before or after.
      - srl_reg: The tool infers an SRL and leaves one register after the SRL.
      - reg_srl: The tool infers an SRL and leaves one register before the SRL.
      - reg_srl_reg: The tool infers an SRL and leaves one register before and one
        register after the SRL.
      - block: The tool infers the SRL inside a block RAM. *)
  module Srl_style : sig
    val register : t
    val srl : t
    val srl_reg : t
    val reg_srl : t
    val reg_srl_reg : t
    val block : t
  end
end
