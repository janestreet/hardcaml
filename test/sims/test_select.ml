open! Base
open Hardcaml
open Ops.Select
module Sim = Cyclesim.With_interface (I) (O)

let create_sim () =
  let sim1 = Sim.create ~implementation:`V1 create in
  let sim2 = Sim.create ~implementation:`V2 create in
  Cyclesim.combine sim1 sim2
;;

let%expect_test "test select" =
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  for _ = 0 to 1_000 do
    List.iter inputs ~f:(fun a -> a := Bits.random ~width:(Bits.width !a));
    Cyclesim.cycle sim
  done
;;
