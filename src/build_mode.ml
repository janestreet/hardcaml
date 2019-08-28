open! Import

type t =
  | Simulation
  | Synthesis
[@@deriving sexp_of]

let of_string = function
  | "simulation" -> Simulation
  | "synthesis" -> Synthesis
  | build_mode -> raise_s [%message "Invalid [Build_mode]" (build_mode : string)]
;;
