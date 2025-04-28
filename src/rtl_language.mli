open! Base

type t =
  | Verilog
  | Vhdl
[@@deriving sexp_of]

(** File extension associated with the language *)
val file_extension : t -> string
