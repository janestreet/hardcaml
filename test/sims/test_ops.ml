open! Base
open Hardcaml
open Ops.Ops
module Sim = Cyclesim.With_interface (I) (O)

let create_sim () =
  let sim1 = Sim.create ~implementation:`V1 create in
  let sim2 = Sim.create ~implementation:`V2 create in
  Cyclesim.combine sim1 sim2
;;

let%expect_test "test operators" =
  let sim = create_sim () in
  let inputs = Cyclesim.inputs sim in
  for _ = 0 to 5_000 do
    List.iter inputs.a ~f:(fun a -> a := Bits.random ~width:(Bits.width !a));
    List.iter inputs.b ~f:(fun b -> b := Bits.random ~width:(Bits.width !b));
    Cyclesim.cycle sim
  done
;;
