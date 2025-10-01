open! Core0

type t =
  | Verilog
  | Systemverilog
  | Vhdl
[@@deriving sexp_of]

let file_extension = function
  | Verilog -> ".v"
  | Systemverilog -> ".sv"
  | Vhdl -> ".vhd"
;;
