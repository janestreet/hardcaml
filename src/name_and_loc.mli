(** A hardcaml signal name with source code location. *)

open! Core0

type t =
  { name : string
  ; loc : Source_code_position.t
  }
[@@deriving bin_io, hash, compare ~localize, equal ~localize, sexp_of]
