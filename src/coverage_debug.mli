(** Module for printing debug spew during a coverage run to a separate file.

    Coverage is often run with the inline tests runner which captures std output.
    Therefore it useful to have debug spew sent somewhere else. *)

open! Core0

(** Print the sexp to the debug file when printing is enabled in the ml file. The lazy
    type ensures that we don't build the sexp unless we are actually going to print it. *)
val maybe_print_debug : Sexp.t Lazy.t -> unit
