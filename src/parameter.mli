(** A [Parameter.t] is the name and value of a configurable attribute of an instantiated
    RTL design.

    In Verilog they are called [parameters]s and in VHDL they are called [generic]s. *)

open Base

module Value : sig
  type t =
    | Bit of bool
    | Bit_vector of Logic.Bit_vector.t
    | Bool of bool
    | Int of int
    | Real of float
    | Std_logic of Logic.Std_logic.t
    | Std_logic_vector of Logic.Std_logic_vector.t
    | Std_ulogic of Logic.Std_logic.t
    | Std_ulogic_vector of Logic.Std_logic_vector.t
    | String of string
  [@@deriving sexp, variants]

  include Equal.S with type t := t
end

type t =
  { name : Parameter_name.t
  ; value : Value.t
  }
[@@deriving sexp_of]

include Equal.S with type t := t

val create : name:string -> value:Value.t -> t
val find_name : t list -> Parameter_name.t -> Value.t Option.t
val find_name_exn : t list -> Parameter_name.t -> Value.t

(** [is_subset ts1 ts2] returns true iff every [t] in [ts1] is in [ts2]. *)
val is_subset : t list -> t list -> bool

val sort_by_name : t list -> t list
