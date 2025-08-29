open! Core0

type t =
  | Vivado
  | Modelsim
[@@deriving sexp_of]

let force_std_logic_generics_to_bits = function
  | Vivado -> true
  | Modelsim -> false
;;
