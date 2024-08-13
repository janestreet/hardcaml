open! Base

type t =
  | Verilog
  | Vhdl
[@@deriving sexp_of]

val file_extension : t -> string
