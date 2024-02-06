module Traced_nodes : sig
  val create
    :  Circuit.t
    -> is_internal_port:(Signal.t -> bool) option
    -> Cyclesim0.Traced.t
end

val create
  :  ?config:Cyclesim0.Config.t
  -> Circuit.t
  -> ((string * Bits.t ref) list, (string * Bits.t ref) list) Cyclesim0.t
