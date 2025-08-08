(** Specify whether to configure the hardware for simulation or synthesis. *)

open Base

type t =
  | Simulation
  | Synthesis
[@@deriving sexp_of, compare ~localize, equal ~localize]

val of_string : string -> t
