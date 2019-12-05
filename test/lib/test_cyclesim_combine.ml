open! Import
open Signal

(* Xor gate *)
let xor_sim () =
  let a, b = input "a" 1, input "b" 1 in
  let c = output "c" (a ^: b) in
  Circuit.create_exn ~name:"xor" [ c ] |> Cyclesim.create
;;

(* Or gate *)
let or_sim () =
  let a, b = input "a" 1, input "b" 1 in
  let c = output "c" (a |: b) in
  Circuit.create_exn ~name:"or" [ c ] |> Cyclesim.create
;;

(* Rename [b] input as [foo] *)
let foo_in_sim () =
  let a, foo = input "a" 1, input "foo" 1 in
  let c = output "c" (a &: foo) in
  Circuit.create_exn ~name:"foo_in" [ c ] |> Cyclesim.create
;;

(* Rename [c] output as [foo] *)
let foo_out_sim () =
  let a, b = input "a" 1, input "b" 1 in
  let c = output "foo" (a |: b) in
  Circuit.create_exn ~name:"foo_out" [ c ] |> Cyclesim.create
;;

(* Add an extra [foo] input *)
let foo_in_superset_sim () =
  let a, b, foo = input "a" 1, input "b" 1, input "foo" 1 in
  let c = output "c" (a &: b &: foo) in
  Circuit.create_exn ~name:"foo_super" [ c ] |> Cyclesim.create
;;

let%expect_test "Port sets differ - inputs" =
  require_does_raise [%here] (fun () -> Cyclesim.combine (xor_sim ()) (foo_in_sim ()));
  [%expect {| ("Input port was not found" (name b)) |}];
  require_does_not_raise [%here] (fun () ->
    ignore
      (Cyclesim.combine ~port_sets_may_differ:true (xor_sim ()) (foo_in_sim ())
       : _ Cyclesim.t));
  [%expect {| |}]
;;

let%expect_test "Port sets differ - outputs" =
  require_does_raise [%here] (fun () -> Cyclesim.combine (xor_sim ()) (foo_out_sim ()));
  [%expect {| ("Output port was not found" (name c)) |}];
  require_does_not_raise [%here] (fun () ->
    ignore
      (Cyclesim.combine ~port_sets_may_differ:true (xor_sim ()) (foo_out_sim ())
       : _ Cyclesim.t));
  [%expect {| |}]
;;

let%expect_test "Port supersets differ" =
  require_does_raise [%here] (fun () ->
    Cyclesim.combine (xor_sim ()) (foo_in_superset_sim ()));
  [%expect {| ("Input port was not found" (name foo)) |}];
  require_does_raise [%here] (fun () ->
    Cyclesim.combine (foo_in_superset_sim ()) (xor_sim ()));
  [%expect {| ("Input port was not found" (name foo)) |}]
;;

let%expect_test "Test comparison" =
  let sim = Cyclesim.combine (xor_sim ()) (or_sim ()) in
  let a, b = Cyclesim.in_port sim "a", Cyclesim.in_port sim "b" in
  require_does_raise [%here] (fun () ->
    (* expected to differ on cycle 3, when both a and b are 1 *)
    for i = 0 to 1 do
      for j = 0 to 1 do
        a := Bits.of_int ~width:1 i;
        b := Bits.of_int ~width:1 j;
        Cyclesim.cycle sim
      done
    done);
  [%expect
    {|
    ("[Cyclesim.combine] output port values differ"
     (error (
       (cycle_no   3)
       (clock_edge Before)
       (port_name  c)
       (value0     0)
       (value1     1)))) |}]
;;
