(** A set of simple counter circuits with different clocking structures. *)

open! Core
open Hardcaml

module I : sig
  type 'a t =
    { clock : 'a
    ; enable : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t = { reg_out : 'a } [@@deriving hardcaml]
end

(** Increment [reg_out] on [clock] if [enable]. *)
val rising_edge : Interface.Create_fn(I)(O).t

(** Increment [reg_out] on [clock]'s falling edge if [enable]. *)
val falling_edge : Interface.Create_fn(I)(O).t

(** Increment [reg_out] on the not of [clock] if [enable]. *)
val inverted_clock : Interface.Create_fn(I)(O).t

(** Increment [reg_out] on the and of [clock] and [enable]. *)
val gated_clock : Interface.Create_fn(I)(O).t

(** Increment [reg_out] if [enable_reg], a reg driven by [enable], with both regs on the
    [clock]'s falling edge. *)
val falling_edge_enable_reg : Interface.Create_fn(I)(O).t
