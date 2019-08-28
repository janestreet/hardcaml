(** RTL attribute specification.  Only relevant to downstream tooling. *)

(** Specification of attributes which may be attached to various objects within a RTL
    design. Such attributes are used to provide implementation hints to down stream CAD
    tools and do not affect any functionality within Hardcaml. *)
type t [@@deriving sexp_of]

(** Attribute value types. *)
module Value : sig
  type t =
    | Int of int
    | String of string
    | Bool of bool
  [@@deriving sexp_of]
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

  (** Select encoding of finite state machine.  Apply to state register. *)
  val fsm_encoding : [ `auto | `gray | `johnson | `none | `one_hot | `sequential ] -> t

  (** Export net for debugging with chipscope. *)
  val mark_debug : bool -> t

  module Ram_style : sig
    val block : t
    val distributed : t
    val registers : t
    val ultra : t
  end
end
