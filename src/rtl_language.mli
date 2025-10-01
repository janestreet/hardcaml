open! Core0

type t =
  | Verilog
  | Systemverilog
  | Vhdl
[@@deriving sexp_of]

(** File extension associated with the language *)
val file_extension : t -> string
