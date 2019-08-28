(** A [Parameter.t] is the name and value of a configurable attribute of an instantiated
    RTL design.

    In Verilog they are called [parameters]s and in VHDL they are called [generic]s. *)

open! Import

(** 9-state VHDL std_logic enumeration *)
module Std_logic : sig
  type t =
    | U (** Uninitialized *)
    | X (** Unknown *)
    | L0 (** Logic 0 *)
    | L1 (** Logic 1 *)
    | Z (** High impedance *)
    | W (** Weak - neither prefer 0 or 1 *)
    | L (** Weak - prefer 0 *)
    | H (** Weak - prefer 1 *)
    | Don't_care (** Dont care *)
  [@@deriving compare, enumerate, sexp_of, variants]

  include Equal.S with type t := t
  module Unstable : Unstable with type t = t

  (** Provide the index of [t] in textual order.  When passing a std_logic parameter from
      verilog to vhdl, we need to encode this type into an integer.  For example, L1 =
      4'd3. *)
  val to_int : t -> int

  (** The OCaml [char] used in [of_char] and [to_char] is the same as used in VHDL. *)
  val of_char_exn : char -> t

  val to_char : t -> char
end

module Std_logic_vector : sig
  type t [@@deriving compare, sexp_of]

  include Equal.S with type t := t
  include Stringable.S with type t := t
  module Unstable : Unstable with type t = t

  val create : Std_logic.t list -> t
  val width : t -> int
  val of_bits : Bits.t -> t
end

module Bit_vector : sig
  type t [@@deriving compare, sexp_of]

  include Equal.S with type t := t
  include Stringable.S with type t := t
  module Unstable : Unstable with type t = t

  val create : bool list -> t
  val width : t -> int
  val of_bits : Bits.t -> t
end

module Value : sig
  type t =
    | Bit of bool
    | Bit_vector of Bit_vector.t
    | Bool of bool
    | Int of int
    | Real of float
    | Std_logic of Std_logic.t
    | Std_logic_vector of Std_logic_vector.t
    | Std_ulogic of Std_logic.t
    | Std_ulogic_vector of Std_logic_vector.t
    | String of string
  [@@deriving compare, sexp_of, variants]

  include Equal.S with type t := t
  module Unstable : Unstable with type t = t
end

type t =
  { name : Parameter_name.t
  ; value : Value.t
  }
[@@deriving compare, sexp_of]

include Equal.S with type t := t
module Unstable : Unstable with type t = t

val create : name:string -> value:Value.t -> t
val find_name : t list -> Parameter_name.t -> Value.t Option.t
val find_name_exn : t list -> Parameter_name.t -> Value.t

(** [is_subset ts1 ts2] returns true iff every [t] in [ts1] is in [ts2]. *)
val is_subset : t list -> t list -> bool

val sort_by_name : t list -> t list
