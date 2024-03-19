open! Import
open Signal

let rtl_write_null outputs = Rtl.print Verilog (Circuit.create_exn ~name:"test" outputs)

let%expect_test "Port names must be unique" =
  require_does_raise [%here] (fun () -> rtl_write_null [ output "a" (input "a" 1) ]);
  [%expect
    {| ("Port names are not unique" (circuit_name test) (input_and_output_names (a))) |}]
;;

let%expect_test "Port names must be legal" =
  require_does_raise [%here] (fun () -> rtl_write_null [ output "a" (input "1^7" 1) ]);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name test)
      (hierarchy_path (test))
      (output ((language Verilog) (mode (To_channel <stdout>))))
      (exn (
        "[Rtl_name.add_port_name] illegal port name"
        (name       1^7)
        (legal_name _1_7)
        (note       "Hardcaml will not change ports names.")
        (port (
          wire
          (names (1^7))
          (width   1)
          (data_in empty))))))
    |}]
;;

let%expect_test "Port name clashes with reserved name" =
  require_does_raise [%here] (fun () ->
    rtl_write_null [ output "generate" (input "x" 1) ]);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name test)
      (hierarchy_path (test))
      (output ((language Verilog) (mode (To_channel <stdout>))))
      (exn (
        "[Rtl_name.add_port_name] port name has already been defined or matches a reserved identifier"
        (port (
          wire
          (names (generate))
          (width   1)
          (data_in x))))))
    |}]
;;

let%expect_test "output wire is width 0 (or empty)" =
  (* The exception is raised by the [output] function. *)
  require_does_raise [%here] (fun () -> rtl_write_null [ output "x" (empty +: empty) ]);
  [%expect
    {|
    ("width of wire was specified as 0" (
      wire (
        wire
        (width   0)
        (data_in empty))))
    |}]
;;

let%expect_test "instantiation input is empty" =
  require_does_raise [%here] (fun () ->
    let a = Signal.empty in
    let inst =
      Instantiation.create () ~name:"example" ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
    in
    Circuit.create_exn ~name:"example" [ Signal.output "b" (Map.find_exn inst "b") ]
    |> Rtl.print Verilog);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name example)
      (hierarchy_path (example))
      (output ((language Verilog) (mode (To_channel <stdout>))))
      (exn (
        "[Rtl_ast] failed to create statement for signal"
        (signal (
          instantiation
          (width 1)
          ("work.example(rtl){the_example}"
            (parameters ())
            (inputs  ((a empty)))
            (outputs ((b 1))))))
        (exn (
          "[Rtl_ast] Failed to find signal in logic map"
          (context "Inst.input_port: a"))))))
    |}]
;;
