open! Core0

type t =
  | Verilog
  | Systemverilog
  | Vhdl
[@@deriving sexp_of, enumerate, to_string]

let file_extension = function
  | Verilog -> ".v"
  | Systemverilog -> ".sv"
  | Vhdl -> ".vhd"
;;
