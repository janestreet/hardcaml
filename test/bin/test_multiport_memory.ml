open! Core
open Hardcaml
open Hardcaml_test

(* Write the example dual port memories to VHDL and Verilog.  They are then compiled
   with Modelsim to check syntax. *)
let () =
  let circuit = Test_multiport_memory.dual_port () in
  Rtl.output Verilog ~output_mode:(To_file "dual_port.v") circuit;
  Rtl.output Vhdl ~output_mode:(To_file "dual_port.vhd") circuit
;;
