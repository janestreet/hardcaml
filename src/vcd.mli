(** VCD (Verilog Change Dump) generation *)

(** wrap a simulator to generate a vcd file *)
val wrap : (string -> unit) -> ('i, 'o) Cyclesim.t -> ('i, 'o) Cyclesim.t
