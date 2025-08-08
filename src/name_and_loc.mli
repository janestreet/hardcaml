(** A hardcaml signal name with source code location. *)

open Base

type t =
  { name : string
  ; loc : Source_code_position.t
  }
[@@deriving hash, compare ~localize, equal ~localize, sexp_of]
