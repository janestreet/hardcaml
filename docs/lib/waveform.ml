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
  let filename = "/tmp/waves.vcd" in
  let oc = Out_channel.open_text filename in
  let sim = Vcd.wrap oc sim in
  testbench sim;
  (* Closing the out channel will ensure the file is flushed to disk *)
  Out_channel.close oc;
  Stdio.print_endline ("Saved waves to " ^ filename)
;;

let%expect_test "vcd generation" =
  test ();
  [%expect {| Saved waves to /tmp/waves.vcd |}]
;;
(* $MDX part-end *)
