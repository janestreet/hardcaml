(** Simple circuit analsysis passes for common issues. *)

open! Import

(** Raises if there exists a seqential element (register or memory) whose clock input pin
    is not in [expected_clock_pins]. Clocks are defined by the name of input clock signals
    into the circuit.
*)
val verify_clock_pins : expected_clock_pins:string list -> Circuit.t -> unit
