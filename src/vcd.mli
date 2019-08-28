(** VCD (Verilog Change Dump) generation *)

(** wrap a simulator to generate a vcd file *)
val wrap : (string -> unit) -> ('i, 'o) Cyclesim.t -> ('i, 'o) Cyclesim.t

(** Drive the gtkwave waveform viewer *)
module Gtkwave : sig
  (** wrap a simulator to generate a vcd file *)
  val wrap : out_channel -> ('i, 'o) Cyclesim.t -> ('i, 'o) Cyclesim.t

  (** launch gtkwave to view the VCD output interactively *)
  val gtkwave : ?args:string -> ('i, 'o) Cyclesim.t -> ('i, 'o) Cyclesim.t
end
