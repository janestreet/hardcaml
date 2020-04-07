(** Used to specify when an operation should be performed - before or after an event like
    a clock edge. *)

type t =
  | Before
  | After
[@@deriving compare, sexp_of]
