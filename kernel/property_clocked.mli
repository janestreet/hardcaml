(** Clocking metadata for properties. *)

open! Core

type 'var t = private
  { edge : Edge.t
  ; clock : 'var
  ; disable_iff : 'var option
  }
[@@deriving sexp_of]

val create : ?disable_iff:'var -> Edge.t -> 'var -> 'var t
