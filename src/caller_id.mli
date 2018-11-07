open! Import

module Mode : sig
  type t =
    | Disabled
    | Top_of_stack
    | Full_trace
end
type t[@@deriving sexp_of]

val set_mode : Mode.t -> unit

val get : ?skip:string list -> unit -> t option
