open! Core0

(** ['a t] represents some kind of circuit memory in a simulation. The contents of the
    memory may change as the simulation advances *)
type 'a t

val of_evsim_memory : 'a Array.t -> 'a t
val of_cyclesim_memory : Cyclesim.Memory.t -> of_bits:(Bits.t -> 'a) -> 'a t
val length : _ t -> int
val get : 'a t -> int -> 'a
