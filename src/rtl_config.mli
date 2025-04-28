open! Base

type t =
  { backend : Rtl_compatibility.t
  ; two_state : bool
  }
[@@deriving sexp_of]

(** Default configuration (backend = Vivado, two_state = false) *)
val default : t
