(** This is simplified version of topsort extracted from the [Topological_sort] library.

    The major change here is that the output order respects the input order as far as
    possible.  This is important for simulation schedules.
*)

open Base

module Edge : sig
  type 'a t =
    { from : 'a
    ; to_ : 'a
    }
end

val sort
  :  nodes:Signal.t list
  -> edges:Signal.t Edge.t list
  -> (Signal.t list, [ `Cycle of Signal.t list ]) Result.t
