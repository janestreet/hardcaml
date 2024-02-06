open Base

type range =
  | Vector of { width : int }
  | Bit
[@@deriving equal, sexp_of]

type reg_or_wire =
  | Reg
  | Wire
[@@deriving equal, sexp_of]

type var =
  { name : string
  ; range : range
  ; reg_or_wire : reg_or_wire
  ; attributes : Rtl_attribute.t list
  ; comment : string option
  }
[@@deriving equal, sexp_of]

type output =
  { output : var
  ; driven_by : var option
  }
[@@deriving sexp_of]

type logic_declaration =
  { read : var
  ; write : var
  ; all_names : var list
  }
[@@deriving sexp_of]

type multiport_memory_declaration =
  { memory : var
  ; memory_type : string
  ; depth : int
  ; range : range
  }
[@@deriving sexp_of]

type declaration =
  | Logic of logic_declaration
  | Multiport_memory of multiport_memory_declaration
  | Inst of logic_declaration
[@@deriving sexp_of]

type binop =
  | Add
  | Sub
  | Mulu
  | Muls
  | And
  | Or
  | Xor
  | Eq
  | Lt
[@@deriving sexp_of]

type assignment =
  | Binop of
      { lhs : var
      ; arg_a : var
      ; op : binop
      ; arg_b : var
      ; signed : bool
      }
  | Not of
      { lhs : var
      ; arg : var
      }
  | Mux of
      { lhs : var
      ; select : var
      ; cases : var list
      }
  | Select of
      { lhs : var
      ; arg : var
      ; high : int
      ; low : int
      }
  | Concat of
      { lhs : var
      ; args : var list
      }
  | Const of
      { lhs : var
      ; constant : Bits.t
      }
  | Wire of
      { lhs : var
      ; driver : var
      }
[@@deriving sexp_of]

type condition =
  | Level of
      { level : Level.t
      ; var : var
      }
  | Edge of
      { edge : Edge.t
      ; var : var
      }
  | Clock of
      { edge : Edge.t
      ; clock : var
      }
[@@deriving sexp_of]

type always =
  | If of
      { condition : condition
      ; on_true : always list
      ; on_false : always list
      }
  | Assignment of
      { lhs : var
      ; rhs : var
      }
  | Memory_assignment of
      { lhs : var
      ; index : var
      ; rhs : var
      }
  | Case of
      { select : var
      ; cases : always list list
      }
[@@deriving sexp_of]

type sensitivity =
  { edge : Edge.t
  ; var : var
  }
[@@deriving sexp_of]

type sensitivity_list =
  | Star
  | Edges of sensitivity list
[@@deriving sexp_of]

type instantiation_input_port =
  { port_name : string
  ; connection : var
  }
[@@deriving sexp_of]

type instantiation_output_port =
  { port_name : string
  ; connection : var
  ; high : int
  ; low : int
  }
[@@deriving sexp_of]

type instantiation =
  { name : string
  ; instance : string
  ; parameters : Parameter.t list
  ; input_ports : instantiation_input_port list
  ; output_ports : instantiation_output_port list
  ; attributes : Rtl_attribute.t list
  }
[@@deriving sexp_of]

type statement =
  | Assignment of assignment
  | Instantiation of instantiation
  | Always of
      { sensitivity_list : sensitivity_list
      ; always : always
      }
  | Mux of
      { to_assignment : unit -> statement
      ; to_always : unit -> statement
      ; is_mux2 : bool
      }
  | Multiport_mem of { always : statement array }
  | Mem_read_port of
      { lhs : var
      ; memory : var
      ; address : var
      }
[@@deriving sexp_of]

type t =
  { name : string
  ; inputs : var list
  ; outputs : output list
  ; declarations : declaration list
  ; statements : statement list
  ; var_map : declaration Map.M(Signal.Uid).t
      (** Map all input, output and internal signals to a var declaration *)
  }
[@@deriving sexp_of]

val of_circuit : blackbox:bool -> Rtl_name.t -> Circuit.t -> t

(** Map signal names to mangled RTL names. This used in [Hardcaml_verilator]. *)
module Signals_name_map : sig
  module Uid_with_index : sig
    type t = Signal.Uid.t * int [@@deriving compare, sexp_of]

    include Comparator.S with type t := t
  end

  type t_rtl_ast = t
  type t = string Map.M(Uid_with_index).t [@@deriving equal, sexp_of]

  val create : t_rtl_ast -> t
end
