(** RTL attribute specification.  Only relevant to downstream tooling. *)

(** Specification of attributes which may be attached to various objects within a RTL
    design. Such attributes are used to provide implementation hints to down stream CAD
    tools and do not affect any functionality within Hardcaml. *)
type t [@@deriving sexp_of, compare, equal, hash]

(** Attribute value types. *)
module Value : sig
  type t =
    | Int of int
    | String of string
    | Bool of bool
  [@@deriving equal, sexp_of]
end

(** Create a new attribute. *)
val create : ?value:Value.t -> string -> t

(** Returns the attribute name *)
val name : t -> string

(** Returns the attribute value, if any. *)
val value : t -> Value.t option

(** A collection of common Xilinx Vivado attributes. *)
module Vivado : sig
  (** Inform Vivado that a registers data input is asychronous to it's clock. *)
  val async_reg : bool -> t

  (** Instruct the synthesizer and place & route tools to keep the node. Cannot be applied
      to a port. *)
  val dont_touch : bool -> t

  (** Setting keep_hierarchy to yes will prevent Vivado from optimizating across module
      boundaries. Can only be applied to modules or instances. *)
  val keep_hierarchy : bool -> t

  (** Select encoding of finite state machine.  Apply to state register. *)
  val fsm_encoding : [ `auto | `gray | `johnson | `none | `one_hot | `sequential ] -> t

  (** Export net for debugging with chipscope. *)
  val mark_debug : bool -> t

  (** Similar to dont_touch *)
  val keep : bool -> t

  (** io_buffer_type Vivado attribute. Applied to top level ports and instructs Vivado not
      to infer the buffer type, instead using the attribute below. *)
  val io_buffer_type : [ `IBUF | `OBUF | `None ] -> t

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
      - block: The tool infers the SRL inside a block RAM
  *)
  module Srl_style : sig
    val register : t
    val srl : t
    val srl_reg : t
    val reg_srl : t
    val reg_srl_reg : t
    val block : t
  end
end
