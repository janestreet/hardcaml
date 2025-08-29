(** Compatibility hacks between different vendor tools. *)

open! Core0

type t =
  | Vivado
  | Modelsim
[@@deriving sexp_of]

(** When instantiating a VHDL component from a Verilog file, how should the resolved
    std_logic type be treated?

    Modelsim has rules for encoding them as a 4 bit integer. Vivado treats them as a bit
    type. *)
val force_std_logic_generics_to_bits : t -> bool
