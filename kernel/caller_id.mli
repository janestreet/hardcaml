(** Optionally embed the callstack in the signal type when it is created.

    When an exception occurs, the callstack is printed as part of the sexp of the signal.
    This is especially useful for dangling wires as it shows the construction site of the
    wire rather than the place that the dangling wire was detected.

    By default it is [Disabled]. Tracing the stack within every signal can become very
    expensive as circuits grow. The environment variable [HARDCAML_DEBUG] can be set to
    enable tracing. *)

open! Core0

module Mode : sig
  type t =
    | Disabled
    | Top_of_stack
    | Coverage_filtered_trace
    (** Condensed full trace that is intended to be useful for code coverage pointers *)
    | Full_trace
end

type t [@@deriving bin_io, sexp_of]

val set_mode : Mode.t -> unit
val get : ?skip:string list -> unit -> t option
val call_stack : t -> Call_stack.t
val call_stack_opt : t option -> Call_stack.t
