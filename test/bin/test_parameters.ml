(* Generate Hardcaml instantiations for VHDL and Verilog to test various parameter types.
   See hardcaml/test/rtl/README. *)

open Core
open Hardcaml
open Hardcaml_test

let () =
  let circ, config = Test_parameter.create_instantiation_test "verilog" in
  Out_channel.write_all
    "test_instantiate_verilog.v"
    ~data:(Rtl.create Verilog [ circ ] ~config |> Rtl.full_hierarchy |> Rope.to_string)
;;

let () =
  let circ, config = Test_parameter.create_instantiation_test "vhdl" in
  Out_channel.write_all
    "test_instantiate_vhdl.vhd"
    ~data:(Rtl.create Vhdl [ circ ] ~config |> Rtl.full_hierarchy |> Rope.to_string)
;;
