(** Specify whether to configure the hardware for simulation or synthesis. *)

open! Import

type t =
  | Simulation
  | Synthesis
[@@deriving sexp_of]

val of_string : string -> t
