open Base

module type Lib = sig
  include Comb.S

  val reg
    :  clock:t
    -> ?clock_edge:Edge.t
    -> ?reset:t
    -> ?reset_edge:Edge.t
    -> ?reset_value:t
    -> ?clear:t
    -> ?clear_value:t
    -> ?enable:t
    -> t
    -> t

  val tristate_buffer : en:t -> i:t -> t:t -> t
end

module type Structural = sig
  type name = string
  type id = int
  type width = int

  type signal =
    | Empty
    (* module interface *)
    | Module_input of id * name * width * Rtl_attribute.t list ref
    | Module_output of id * name * width * signal ref * Rtl_attribute.t list ref
    | Module_tristate of id * name * width * signal list ref * Rtl_attribute.t list ref
    (* internal wires *)
    | Internal_wire of id * width * signal ref
    | Internal_triwire of id * width * signal list ref
    (* instantiations *)
    | Instantiation_output of id * name (** reference to instantiation *)
    | Instantiation_tristate of id * name
    | Instantiation of
        id
        * name
        * (string * generic) list
        * (string * signal) list (* inputs (read) *)
        * (string * signal) list (* outputs (write; drive wires/module outputs *)
        * (string * signal) list (* tristate (write; drive triwires/module tristates *)
        * string option
        * Rtl_attribute.t list
    (* basic RTL operators *)
    | Rtl_op of id * width * rtl_op

  and rtl_op =
    | Constant of string
    | Select of int * int * signal
    | Concat of signal list
    | Mux of signal * signal list

  and generic =
    | GInt of int
    | GFloat of float
    | GString of string
    | GUnquoted of string

  type circuit

  (** [Structural_rtl_component]s are generated by the [Comb] api.

      They may be converted into standard hardcaml circuits for code generation.
  *)
  module Structural_rtl_component : sig
    type t [@@deriving compare, sexp_of]

    include Comparator.S with type t := t

    val rtl_circuit : t -> Circuit.t
  end

  exception Invalid_submodule_input_connection of string * string * signal
  exception Invalid_submodule_output_connection of string * string * signal
  exception Invalid_submodule_tristate_connection of string * string * signal
  exception Wire_already_assigned of signal
  exception Invalid_assignment_target of signal
  exception Cant_assign_wire_with of signal
  exception Cant_assign_triwire_with of signal
  exception Invalid_name of signal
  exception Invalid_width of signal
  exception Invalid_id of signal
  exception Invalid_constant of string
  exception Rtl_op_arg_not_readable of signal
  exception Too_few_mux_data_elements
  exception Too_many_mux_data_elements of int
  exception All_mux_data_elements_must_be_same_width of int list
  exception No_elements_to_concat
  exception Select_index_error of int * int
  exception Binop_arg_widths_different of string
  exception No_circuit
  exception Circuit_already_started

  (** Clears the circuit database and resets all internal state back to initial values. *)
  val reset_circuit_database : unit -> unit

  (** start circuit *)
  val start_circuit : string -> unit

  (** complete circuit, add to database *)
  val end_circuit : unit -> unit

  (** find circuit in database *)
  val find_circuit : string -> circuit

  val create_circuit : string -> (unit -> unit) -> circuit

  (** Return the circuit name *)
  val circuit_name : circuit -> string

  val structural_rtl_components : circuit -> Set.M(Structural_rtl_component).t

  (** Add an attribute to the signal. Currently only works on input and outputs. *)
  val add_attribute : signal -> Rtl_attribute.t -> unit

  val width : signal -> int
  val mk_input : string -> int -> signal
  val mk_output : string -> int -> signal
  val mk_tristate : string -> int -> signal
  val mk_wire : int -> signal
  val mk_triwire : int -> signal
  val ( <== ) : signal -> signal -> unit
  val is_connected : signal -> bool

  val inst
    :  ?instance_name:string
    -> ?attributes:Rtl_attribute.t list
    -> ?g:(string * generic) list
    -> ?i:(string * signal) list
    -> ?o:(string * signal) list
    -> ?t:(string * signal) list
    -> string
    -> unit

  val ( ==> ) : 'a -> 'b -> 'a * 'b
  val of_bit_string : string -> signal
  val z : int -> signal
  val mux : signal -> signal list -> signal
  val concat_msb : signal list -> signal
  val select : signal -> high:int -> low:int -> signal
  val prefix : string

  (** Design API (must be rebuilt between each circuit) *)
  module type Lib = Lib with type t = signal

  module Lib () : Lib

  val write_verilog : (string -> unit) -> circuit -> unit

  module With_interface (I : Interface.S) (O : Interface.S) (T : Interface.S) : sig
    val create_circuit
      :  string
      -> (signal I.t -> signal O.t -> signal T.t -> unit)
      -> circuit

    val inst
      :  ?instance_name:string
      -> ?attributes:Rtl_attribute.t list
      -> ?g:(string * generic) list
      -> string
      -> signal I.t
      -> signal O.t
      -> signal T.t
      -> unit
  end
end
