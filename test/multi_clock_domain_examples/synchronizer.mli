(** Basic double flop synchronizer.

    Registers first in the source clock domain which you would not necessarily need to do,
    but is helpful for showing the behaviour of the synchronizer. *)

open! Core
open Hardcaml

module I : sig
  type 'a t =
    { clock_src : 'a
    ; clock_dst : 'a
    ; d : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { q : 'a } [@@deriving hardcaml]
end

val create : Interface.Create_fn(I)(O).t
