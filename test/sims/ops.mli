(** Construct circuits which instantiate the basic Hardcaml operations over a variety of
    operand sizes.

    Can be used to compare 2 simulators together for correctness. *)

open Base
open Hardcaml

val bit_sizes : int list
val num_bit_sizes : int

module Sizes : Interface.S with type 'a t = 'a list
module Sizes1 : Interface.S with type 'a t = 'a list
module Sizes_mul : Interface.S with type 'a t = 'a list

module I : sig
  type 'a t =
    { a : 'a Sizes.t
    ; b : 'a Sizes.t
    }
  [@@deriving hardcaml]
end

module Ops : sig
  module I = I

  module O : sig
    type 'a t =
      { add : 'a Sizes.t
      ; sub : 'a Sizes.t
      ; and_ : 'a Sizes.t
      ; or_ : 'a Sizes.t
      ; xor_ : 'a Sizes.t
      ; not_ : 'a Sizes.t
      ; eq : 'a Sizes1.t
      ; lt : 'a Sizes1.t
      }
    [@@deriving hardcaml]
  end

  val create : Interface.Create_fn(I)(O).t
  val circuit : unit -> Circuit.t
end

module Mul : sig
  module I = I

  module O : sig
    type 'a t =
      { mulu : 'a Sizes.t
      ; muls : 'a Sizes.t
      }
    [@@deriving hardcaml]
  end

  val create : Interface.Create_fn(I)(O).t
  val circuit : unit -> Circuit.t
end

module type Test_arg = sig
  val random_state : Random.State.t
  val number_of_ops : int
end

(* 200 operations with a default seed *)
module Default_test : Test_arg

module Make_concat (X : Test_arg) : sig
  module I = Sizes
  module O : Interface.S with type 'a t = 'a list

  val create : Interface.Create_fn(I)(O).t
  val circuit : unit -> Circuit.t
end

module Concat : module type of Make_concat (Default_test)

module Make_select (X : Test_arg) : sig
  module I = Sizes
  module O : Interface.S with type 'a t = 'a list

  val create : Interface.Create_fn(I)(O).t
  val circuit : unit -> Circuit.t
end

module Select : module type of Make_select (Default_test)
