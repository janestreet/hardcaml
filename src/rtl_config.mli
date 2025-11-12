open! Core0

type t =
  { backend : Rtl_compatibility.t
  ; two_state : bool
  ; mangle_io_port_names : bool
  }
[@@deriving sexp_of]

(** Default configuration (backend = Vivado, two_state = false, mangle_io_port_names =
    false) *)
val default : t
