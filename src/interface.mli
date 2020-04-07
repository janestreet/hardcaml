(** Interfaces specify the widths and names of a group of signals, and some functions for
    manipulating the signals as a group.

    They are generally used with [ppx_deriving_hardcaml] as follows

    {[
      type t = { ... } [@@deriving sexp_of, hardcaml]
    ]}

    The [sexp_of] is required, and must appear before [hardcaml].  This syntax
    generates a call to [Interface.Make], which therefore does not need to be
    explicitly called. *)

include Interface_intf.Interface (** @inline *)
