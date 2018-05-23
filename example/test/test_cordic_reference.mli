open! Import

type polar =
  { magnitude : float
  ; phase : float }

module Rect : sig
  type t =
    { x : float
    ; y : float }
  [@@deriving sexp_of]

  val to_polar : t -> polar
end

module Polar : sig
  type t = polar =
    { magnitude : float
    ; phase : float }
  [@@deriving sexp_of]

  val to_rect : t -> Rect.t
end
