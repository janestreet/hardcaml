open! Base
open Hardcaml
open Signal
open Expect_test_helpers_base

let%expect_test "file IO" =
  let output output_mode =
    Rtl.output
      ~output_mode
      Verilog
      (Circuit.create_exn ~name:"test" [ output "x" (input "y" 1) ])
  in
  require_does_raise [%here] (fun () -> output (To_file "/foo"));
  [%expect
    {|
    ("Error while initializing output mode."
      (circuit_name test)
      (language     Verilog)
      (output_mode (To_file   /foo))
      (exn         (Sys_error "/foo: Permission denied")))
    |}];
  require_does_raise [%here] (fun () -> output (In_directory "/foo"));
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name test)
      (hierarchy_path (test))
      (output ((language Verilog) (mode (In_directory /foo))))
      (exn (Sys_error "/foo/test.v: No such file or directory")))
    |}]
;;
