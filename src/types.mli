module Clocking = Clocking

module Cross_product (Outer : Interface.S) (Inner : Interface.S) :
  Cross_product.S with module Inner := Inner and module Outer := Outer

module Pair : Pair_intf.Pair
module With_valid : With_valid_intf.With_valid

module type Value_arg = Value.Arg

(** An interface for a single value *)
module Value (S : Value_arg) : Interface.S with type 'a t = 'a

module type Scalar = Scalar.S

module Scalar (S : Value_arg) : Scalar with type 'a t = 'a

module type Arg_with_length = sig
  include Value_arg

  val length : int
end

module List (A : Arg_with_length) : Interface.S with type 'a t = 'a list
module Array (A : Arg_with_length) : Interface.S with type 'a t = 'a array
