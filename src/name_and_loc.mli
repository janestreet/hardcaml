(** A hardcaml signal name with source code location. *)

open Base

type t =
  { name : string
  ; loc : Source_code_position.t
  }
[@@deriving sexp_of]
