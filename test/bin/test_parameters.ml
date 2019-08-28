(* Generate Hardcaml instantiations for VHDL and Verilog to test various
   parameter types.  See hardcaml/test/rtl/README. *)

open! Import

let () =
  Out_channel.with_file "test_instantiate_verilog.v" ~f:(fun f ->
    Rtl.output
      Verilog
      ~output_mode:(To_channel f)
      (Test_parameter.create_instantiation_test "verilog"))
;;

let () =
  Out_channel.with_file "test_instantiate_vhdl.vhd" ~f:(fun f ->
    Rtl.output
      Vhdl
      ~output_mode:(To_channel f)
      (Test_parameter.create_instantiation_test "vhdl"))
;;
