(* generate a small vcd file as an expect test *)

open! Import

let%expect_test "simple vcd file" =
  let module Sim = Cyclesim in
  let module S = Cyclesim in
  let open Signal in
  let reg_spec = Reg_spec.create () ~clock ~clear in
  let a, b = input "a" 8, input "b" 8 in
  let c, d =
    reg reg_spec ~enable:vdd (a +: b), pipeline reg_spec ~enable:vdd ~n:2 (a -: b)
  in
  let c, d = output "c" c, output "d" d in
  let circ = Circuit.create_exn ~name:"test" [ c; d ] in
  let sim = Sim.create circ in
  let sim = Vcd.wrap print_string sim in
  let a, b = S.in_port sim "a", S.in_port sim "b" in
  for i = 0 to 2 do
    for j = 0 to 2 do
      a := Bits.of_int ~width:8 (i * 10);
      b := Bits.of_int ~width:8 (j * 10);
      S.cycle sim
    done
  done;
  [%expect
    {|
    $date
      ...
    $end
    $version
      Hardcaml
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! clock $end
    $var wire 1 " reset $end
    $var wire 1 # clear $end
    $var wire 8 % b $end
    $var wire 8 & a $end
    $upscope $end
    $scope module outputs $end
    $var wire 8 ' c $end
    $var wire 8 ( d $end
    $upscope $end
    $scope module various $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    x#
    bxxxxxxxx %
    bxxxxxxxx &
    bxxxxxxxx '
    bxxxxxxxx (
    $end
    #0
    1!
    0"
    0#
    b00000000 %
    b00000000 &
    b00000000 '
    b00000000 (
    #5
    0!
    #10
    1!
    0"
    b00001010 %
    #15
    0!
    #20
    1!
    0"
    b00010100 %
    b00001010 '
    #25
    0!
    #30
    1!
    0"
    b00000000 %
    b00001010 &
    b00010100 '
    b11110110 (
    #35
    0!
    #40
    1!
    0"
    b00001010 %
    b00001010 '
    b11101100 (
    #45
    0!
    #50
    1!
    0"
    b00010100 %
    b00010100 '
    b00001010 (
    #55
    0!
    #60
    1!
    0"
    b00000000 %
    b00010100 &
    b00011110 '
    b00000000 (
    #65
    0!
    #70
    1!
    0"
    b00001010 %
    b00010100 '
    b11110110 (
    #75
    0!
    #80
    1!
    0"
    b00010100 %
    b00011110 '
    b00010100 (
    #85
    0! |}]
;;
