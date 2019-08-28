(** Constant propagation tests. *)
open! Import

module Trace : sig
  (* Types of traced functions with 1, 2 or 3 arguments *)
  type 'a fn1 = 'a * Signal.t [@@deriving sexp_of]
  type ('a, 'b) fn2 = ('a * 'b) * Signal.t [@@deriving sexp_of]
  type ('a, 'b, 'c) fn3 = ('a * 'b * 'c) * Signal.t [@@deriving sexp_of]
  type signal = Signal.t [@@deriving sexp_of]

  (* Wrap function so that arguments are traced with the result *)
  val fn1 : ('a -> Signal.t) -> 'a -> 'a fn1
  val fn2 : ('a -> 'b -> Signal.t) -> 'a -> 'b -> ('a, 'b) fn2
  val fn3 : ('a -> 'b -> 'c -> Signal.t) -> 'a -> 'b -> 'c -> ('a, 'b, 'c) fn3

  (* Exhaustively test unary and binary operators at given bit width *)
  val op1 : int -> f:(Signal.t -> 'a) -> 'a list
  val op2 : int -> f:(Signal.t -> Signal.t -> 'a) -> 'a list

  val binary_op_tests
    :  string
    -> (Signal.t -> Signal.t -> Signal.t)
    -> (Signal.t -> int -> Signal.t)
    -> Sexp.t

  val binary_op_tests_no_rhs_int : string -> (Signal.t -> Signal.t -> Signal.t) -> Sexp.t

  val binary_op_tests_with_one_constant
    :  string
    -> (Signal.t -> Signal.t -> Signal.t)
    -> unit

  val mul_op_tests : string -> (Signal.t -> Signal.t -> Signal.t) -> Sexp.t
end
