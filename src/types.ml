open! Core
module Clocking = Clocking
module Cross_product = Cross_product.Make

module type Value_arg = Value.Arg

module Pair = Pair
module With_valid = With_valid
module Value = Value.Make

module type Scalar = Scalar.S

module Scalar = Scalar.Make
