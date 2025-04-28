open Base

type t =
  { name : string
  ; loc : Source_code_position.t
  }

(* Custom implementation to generate a compact representation for the type. *)
let sexp_of_t { name; loc } = [%sexp ((name, loc) : string * Source_code_position.t)]
