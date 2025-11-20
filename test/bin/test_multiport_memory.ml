open! Core
open Hardcaml
open Hardcaml_test

(* Write the example dual port memories to VHDL and Verilog. They are then compiled with
   Modelsim to check syntax. *)
let () =
  let circuit = Test_multiport_memory.dual_port () in
  Out_channel.write_all
    "dual_port.v"
    ~data:(Rtl.create Verilog [ circuit ] |> Rtl.full_hierarchy |> Rope.to_string);
  Out_channel.write_all
    "dual_port.vhd"
    ~data:(Rtl.create Vhdl [ circuit ] |> Rtl.full_hierarchy |> Rope.to_string)
;;
