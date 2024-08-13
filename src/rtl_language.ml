open! Base

type t =
  | Verilog
  | Vhdl
[@@deriving sexp_of]

let file_extension = function
  | Verilog -> ".v"
  | Vhdl -> ".vhd"
;;
