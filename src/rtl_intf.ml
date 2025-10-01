open! Core0

(** Generate RTL for one or more circuits.

    [create] returns a list of [Hierarchical_circuits] which express the complete design
    hierarchy (as found by querying the [Circuit_database]). This can be converted to RTL
    by either explicitly traversing the circuit hierarchy and calling
    [Circuit_instance.rtl] or by using one of [full_hierarchy], [top_levels_only],
    [top_levels_as_blackboxes] or [top_levels_and_blackboxes] as required.

    Generated RTL is returned as a [Rope] datastructure which can be converted to a string
    for file IO. *)
module type Rtl = sig
  module Language = Rtl_language

  module Circuit_instance : sig
    type t

    (** Get underlying hardcaml circuit. *)
    val circuit : t -> Circuit.t

    (** Name of circuit *)
    val module_name : t -> string

    (** Circuit RTL implementation. *)
    val rtl : t -> Rope.t

    (** Circuit as a black box instance. *)
    val blackbox : t -> Rope.t

    (** Mapping between signals and signal names. *)
    val name_map : t -> Rtl_ast.Signals_name_map.t
  end

  (** Generated top levels and sub circuits. *)
  module Hierarchical_circuits : sig
    type t =
      { subcircuits : t list
      ; top : Circuit_instance.t
      }

    (** List of sub circuits implementing full design hierarchy *)
    val subcircuits : t list -> Circuit_instance.t list

    (** List of sub circuits instantiated only by the top level designs. *)
    val top_level_subcircuits : t list -> Circuit_instance.t list

    (** List of top level modules *)
    val top : t list -> Circuit_instance.t list
  end

  (** Create a hierarchical set of circuit instances that can be traversed to get the RTL
      implementation(s). *)
  val create
    :  ?database:Circuit_database.t
    -> ?config:Rtl_config.t
    -> Language.t
    -> Circuit.t list
    -> Hierarchical_circuits.t list

  (** Create RTL for the top level circuits and everything it instantiates. *)
  val full_hierarchy : Hierarchical_circuits.t list -> Rope.t

  (** Create RTL just for the top level circuits. *)
  val top_levels_only : Hierarchical_circuits.t list -> Rope.t

  (** Create blackboxes of the top level circuits. *)
  val top_levels_as_blackboxes : Hierarchical_circuits.t list -> Rope.t

  (** Create RTL for the top level circuits, and black boxes for the things they
      instantiate. *)
  val top_levels_and_blackboxes : Hierarchical_circuits.t list -> Rope.t

  (** [print] RTL for a circuit, with optional hierarchy (if [database] is provided), to
      stdout. *)
  val print
    :  ?database:Circuit_database.t
    -> ?config:Rtl_config.t
    -> Language.t
    -> Circuit.t
    -> unit

  module Digest : sig
    type t [@@deriving sexp_of]

    val create : Rope.t -> t
    val to_string : t -> String.t
    val to_constant : t -> Constant.t
  end
end
