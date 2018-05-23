(** Reference implementation of the CORDIC algorithm using [Float.t]. *)

open! Import

(** Determines the cordic update equations *)
module System : sig
  type t =
    | Circular
    | Linear
    | Hyperbolic
end

(** iteration mode *)
module Mode : sig
  type t =
    | Rotation
    | Vectoring
    | Inverse
end

val iter
  :  iterations : int
  -> init : 'a
  -> f : (i:int -> ih:int -> 'a -> 'a)
  -> 'a

val gain : iterations : int -> float

val gainh : iterations : int -> float

val cordic
  :  ?c : float (* rotation target in [Inverse] mode. *)
  -> unit
  -> system : System.t
  -> mode : Mode.t
  -> iterations : int
  -> x : float
  -> y : float
  -> z : float
  -> float * float * float

val cos_sin       : iterations : int -> float -> float * float

val polar_to_rect : iterations : int -> float -> float -> float * float

val rotate_vector : iterations : int -> float -> float -> float -> float * float

val atan          : iterations : int -> float -> float

val atan2         : iterations : int -> float -> float -> float

val rect_to_polar : iterations : int -> float -> float -> float * float

val asin          : iterations : int -> float -> float

val mul           : iterations : int -> float -> float -> float

val div           : iterations : int -> float -> float -> float

val cosh_sinh     : iterations : int -> float -> float * float

val atanh         : iterations : int -> float -> float
