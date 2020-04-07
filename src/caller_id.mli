(** Embed the callstack in the signal type when it is created.

    When an exception occurs, the callstack is printed as part of the sexp of the signal.
    This is especially useful for dangling wires as it shows the construction site of the
    wire rather than the place that the dangling wire was detected.

    By default it is not enabled. *)

open! Import

module Mode : sig
  type t =
    | Disabled
    | Top_of_stack
    | Full_trace
end

type t [@@deriving sexp_of]

val set_mode : Mode.t -> unit
val get : ?skip:string list -> unit -> t option
