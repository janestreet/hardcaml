open Base

module Combine_error : sig
  type t =
    { cycle_no : int
    ; clock_edge : Side.t
    ; port_name : string
    ; value0 : Bits.t
    ; value1 : Bits.t
    }
  [@@deriving sexp_of]
end

(** Combine 2 simulators.  The inputs are set on the 1st simulator and copied to the
    2nd.  Outputs are checked and [on_error] is called if a difference is found.  By
    default, [on_error] raises.

    The simulators should have the same input and output port sets, unless
    [port_sets_may_differ] is [true], in which case only ports which exist on both
    simulators are checked. *)
val combine
  :  ?port_sets_may_differ:bool (** Default is [false]. *)
  -> ?on_error:(Combine_error.t -> unit)
  -> ('i, 'o) Cyclesim0.t
  -> ('i, 'o) Cyclesim0.t
  -> ('i, 'o) Cyclesim0.t
