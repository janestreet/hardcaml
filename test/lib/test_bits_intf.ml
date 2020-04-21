(** Test routines for the various vector calculations in [Bits.Comb] **)

open! Import

(** Test function called to check a result and provide an error message.  In expect tests,
    this is a thin wrapper around [Expect_test_helpers_core.require].  In long-running
    regression tests in ../regression, this prints to a log. *)
module type Require = sig
  val require : Source_code_position.t -> bool -> error_message:Sexp.t Lazy.t -> unit
end

module type Test_ = sig
  type config

  (** [test] tests each primitive in [config.prims] [config.iterations] times, stopping
      iteration for a particular primitive at the first error if
      [stop_on_first_primitive_error]. *)
  val test
    :  ?stop_on_first_primitive_error:bool (** default is [true] *)
    -> Source_code_position.t
    -> config
    -> unit
end

module type Test_bits = sig
  (** The following are operations from [Comb.Primitives].  The full [Comb.S] signature is
      derived from these primitives. *)
  module Primitive_op : sig
    type t =
      | Add
      | Sub
      | Mulu
      | Muls
      | And
      | Or
      | Xor
      | Not
      | Eq
      | Lt
      | Sel
      | Mux
      | Cat
    [@@deriving enumerate, sexp_of]

    val name : t -> string
    val arg_type : t Command.Arg_type.t
  end

  (** One constructor per (tested) module in [Bits.Comb] with an associated test name. *)
  module Bits_module : sig
    type t =
      | IntbitsList
      | Bits
      | Bits_int_array
      | Mutable_Bits_int_array
      (** Special module which breaks a few primtives by inverting the result.  Used to
          test the test-framework itself. *)
      | BadPrimitives
    [@@deriving sexp_of, enumerate]

    val arg_type : t Command.Arg_type.t
    val module_ : t -> (module Comb.S)
    val name : t -> string
    val short_name : t -> string
  end

  module Config : sig
    type t =
      { bits1 : Bits_module.t
      ; bits2 : Bits_module.t
      ; prims : Primitive_op.t list
      ; iterations : int
      ; min_bit_width : int
      ; max_bit_width : int
      }
    [@@deriving sexp_of]
  end

  module type Require = Require
  module type Test = Test_ with type config := Config.t

  module Make (R : Require) : Test
  module Test : Test
end
