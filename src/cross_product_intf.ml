(** Functor to create a Hardcaml Interface from a cross-product of two
    types. *)

module type S = sig
  module Inner : Interface.S
  module Outer : Interface.S

  module type S = Interface.S with type 'a t = 'a Inner.t Outer.t

  include S
end

module type Cross_product = sig
  module type S = S

  module Make (Outer : Interface.S) (Inner : Interface.S) :
    S with module Inner := Inner and module Outer := Outer
end
