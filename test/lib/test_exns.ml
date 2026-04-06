open Core
open Hardcaml
open Signal

let%expect_test "exns" =
  let a = input "a" 1 in
  print_s [%message (vdd |: a : Signal.t)];
  [%expect {| ("vdd |: a" (const (width 1) (value 0b1))) |}];
  print_s [%message (Unoptimized.(vdd |: a) : Signal.t)];
  [%expect {| ("let open Unoptimized in vdd |: a" (or (width 1) (arguments (0b1 a)))) |}];
  Expect_test_helpers_base.require_does_raise (fun () ->
    Circuit.create_exn ~name:"foo" [ output "o" (a +: wire 1) ]);
  [%expect
    {|
    ("circuit input signal must have a port name (unassigned wire?)"
     (input_signal (wire (width 1))))
    |}];
  let x = wire 1 in
  let y = a |: x in
  x <-- y;
  Expect_test_helpers_base.require_does_raise (fun () ->
    Circuit.create_exn ~name:"foo" [ output "q" x ]);
  [%expect
    {|
    ("Combinational loop" (
      (or (width 1) (arguments (a wire)))
      (wire
        (width   1)
        (data_in or))))
    |}];
  let circ =
    Circuit.create_exn
      ~config:{ Circuit.Config.default with detect_combinational_loops = false }
      ~name:"foo"
      [ output "q" x ]
  in
  Expect_test_helpers_base.require_does_raise (fun () -> Cyclesim.create circ);
  [%expect
    {|
    ("Combinational loop" (
      (or (width 1) (arguments (a wire)))
      (wire
        (width   1)
        (data_in or))))
    |}]
;;
