open! Import

type 'a min_max =
  { min : 'a
  ; max : 'a
  }
[@@deriving sexp_of]

type of_int = (int * int) * Signal.t [@@deriving sexp_of]

val of_int : int -> int -> of_int
val min_max : (int -> 'a -> 'b) -> (module Int.S with type t = 'a) -> 'b min_max
val sexp_of_const_signal : ?depth:int -> Signal.t -> Sexp.t
