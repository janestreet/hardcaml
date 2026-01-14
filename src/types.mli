module Clocking = Clocking

module Cross_product (Outer : Interface.S) (Inner : Interface.S) :
  Cross_product.S with module Inner := Inner and module Outer := Outer

module Pair : Pair_intf.Pair
module With_valid : With_valid_intf.With_valid

module type Value_arg = Value.Arg

val value : ?wave_format:Wave_format.t -> ?name:string -> int -> (module Value.S)

module type Value = Value.S

(** An interface for a single value *)
module Value (S : Value_arg) : Value.S

val scalar
  :  ?wave_format:Wave_format.t
  -> ?name:string
  -> int
  -> (module Scalar.S_untyped)

module type Scalar = Scalar.S

(** An interface for a single value.

    Generating a Scalar results in an 'untyped' value such that ['a t = 'a]. However, when
    exposed via an interface it must be [Scalar.S] which hides this equality. *)
module Scalar (S : Value_arg) : Scalar.S_untyped

module type Arg_with_length = sig
  include Value_arg

  val length : int
end

module type Interface_with_length = sig
  include Interface.Pre

  val length : int
end

module List (A : Arg_with_length) : Interface.S with type 'a t = 'a list

module Interface_list (X : Interface_with_length) :
  Interface.S with type 'a t = 'a X.t list

module Array (A : Arg_with_length) : Interface.S with type 'a t = 'a array

module Interface_array (X : Interface_with_length) :
  Interface.S with type 'a t = 'a X.t array
