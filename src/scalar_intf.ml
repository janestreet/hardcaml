open Base

module type Arg = sig
  val port_name : string
  val port_width : int
end

module type S = sig
  include Interface.S

  val num_bits : int
  val create : (module Comb.S with type t = 'a) -> 'a -> 'a t
  val unwrap : 'a t -> 'a

  module Unsafe : sig
    val wrap : 'a -> 'a t
  end

  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

module type S_untyped = S with type 'a t = 'a

module type Scalar = sig
  module type Arg = Arg
  module type S = S
  module type S_untyped = S_untyped

  module Make (X : Arg) : S_untyped
end
