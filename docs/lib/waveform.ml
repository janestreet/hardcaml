open Base
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; incr : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { dout : 'a [@bits 8] } [@@deriving hardcaml]
end

let create (i : _ I.t) =
  { O.dout =
      reg_fb
        (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
        ~enable:i.incr
        ~width:8
        ~f:(fun d -> d +:. 1)
  }
;;

module Simulator = Cyclesim.With_interface (I) (O)

let testbench sim =
  let inputs : _ I.t = Cyclesim.inputs sim in
  let step ~clear ~incr =
    inputs.clear := if clear = 1 then Bits.vdd else Bits.gnd;
    inputs.incr := if incr = 1 then Bits.vdd else Bits.gnd;
    Cyclesim.cycle sim
  in
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:1;
  step ~clear:0 ~incr:1;
  step ~clear:1 ~incr:0;
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:0
;;

(* $MDX part-begin=vcd *)
let test () =
  let sim = Simulator.create create in
  let sim = Vcd.wrap Stdio.stdout sim in
  testbench sim
;;

let%expect_test "vcd generation" =
  test ();
  [%expect
    {|
    $date
      ...
    $end
    $version
      hardcaml-cyclesim
    $end
    $comment
      Hardware design in ocaml
    $end
    $timescale 1ns $end
    $scope module inputs $end
    $var wire 1 ! -clock $end
    $var wire 1 " -reset $end
    $var wire 1 $ clear $end
    $var wire 1 # incr $end
    $upscope $end
    $scope module outputs $end
    $var wire 8 & dout $end
    $upscope $end
    $scope module various $end
    $upscope $end
    $enddefinitions $end
    $dumpvars
    x!
    x"
    x$
    x#
    bxxxxxxxx &
    $end
    #0
    1!
    0"
    0#
    0$
    b00000000 &
    #5
    0!
    #10
    1!
    0"
    1#
    #15
    0!
    #20
    1!
    0"
    b00000001 &
    #25
    0!
    #30
    1!
    0"
    0#
    1$
    b00000010 &
    #35
    0!
    #40
    1!
    0"
    0$
    b00000000 &
    #45
    0!
    #50
    1!
    0"
    #55
    0!
    |}]
;;
(* $MDX part-end *)
