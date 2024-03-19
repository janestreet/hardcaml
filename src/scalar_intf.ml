open Base

module type S = sig
  include Interface.S

  val num_bits : int
  val create : (module Comb.S with type t = 'a) -> 'a -> 'a t
  val unwrap : 'a t -> 'a

  module Unsafe : sig
    val wrap : 'a -> 'a t
  end

  val apply : ('a -> 'b) t -> 'a t -> 'b t

  val to_with_valid
    :  (module Comb.S with type t = 'a)
    -> 'a With_valid.t
    -> 'a With_valid.t t

  val from_with_valid
    :  (module Comb.S with type t = 'a)
    -> 'a With_valid.t t
    -> 'a With_valid.t
end

module type S_untyped = S with type 'a t = 'a

module type Scalar = sig
  module type S = S
  module type S_untyped = S_untyped

  module Make (X : Value.Arg) : S_untyped
end
