open! Import
open Signal

let test o =
  print_s
    [%sexp
      (Signal_graph.create o |> Signal_graph.detect_combinational_loops
       : unit Or_error.t)]
;;

let%expect_test "no loop" =
  let a, b = input "a" 2, input "b" 2 in
  let c, d = a +: b, a -: b +:. 1 in
  test [ c; d ];
  [%expect {| (Ok ()) |}]
;;

let%expect_test "constant assigned to a wire" =
  let w = wire 1 in
  w <== gnd;
  test [ w ];
  [%expect {| (Ok ()) |}]
;;

let%expect_test "[wireof] constant" =
  let w = wireof vdd -- "output" in
  test [ w ];
  [%expect {| (Ok ()) |}]
;;

let%expect_test "looping wire" =
  let w = wire 1 in
  w <== w;
  test [ w ];
  [%expect
    {|
    (Error (
      "combinational loop" (
        through_signal (
          wire
          (width   1)
          (data_in wire))))) |}]
;;

let%expect_test "combinational loop" =
  let a = input "a" 2 in
  let w = wire 2 in
  let b = a +: w in
  w <== b;
  test [ b ];
  [%expect
    {|
    (Error (
      "combinational loop" (through_signal (add (width 2) (arguments (a wire)))))) |}]
;;

let%expect_test "long combinational loop through logic" =
  let a = input "a" 2 in
  let b = input "b" 2 in
  let c = input "c" 2 in
  let w = wire 2 in
  let d = w +: a |: b &: c in
  let e = mux2 (lsbs a) (d +: c) (a |: b) in
  let f = d &: e in
  let g = reduce ~f:( ^: ) [ d; e; f ] in
  w <== g;
  (* loop occurs at all points in the logic *)
  test [ d ];
  [%expect
    {|
    (Error (
      "combinational loop" (through_signal (and (width 2) (arguments (or c)))))) |}];
  test [ e ];
  [%expect
    {|
    (Error (
      "combinational loop" (through_signal (and (width 2) (arguments (or c)))))) |}];
  test [ f ];
  [%expect
    {|
    (Error (
      "combinational loop" (through_signal (and (width 2) (arguments (or c)))))) |}];
  test [ g ];
  [%expect
    {|
    (Error (
      "combinational loop" (through_signal (xor (width 2) (arguments (xor and)))))) |}]
;;

let%expect_test "combinational loop in 2nd arg" =
  let a = input "a" 2 in
  let w = wire 2 in
  let b = a +: w in
  w <== b;
  test [ a -:. 1; b ];
  [%expect
    {|
    (Error (
      "combinational loop" (through_signal (add (width 2) (arguments (a wire)))))) |}]
;;

let%expect_test "loop through register" =
  let a =
    Signal.reg_fb (Reg_spec.create () ~clock) ~enable:vdd ~w:2 (fun d -> d +:. 1)
  in
  test [ a ];
  [%expect {|
    (Ok ()) |}]
;;

let%expect_test "loop through 2 registers" =
  let reg_spec = Reg_spec.create () ~clock in
  let a = input "a" 2 in
  let w = wire 2 in
  let d = a +: w in
  let d = Signal.reg reg_spec ~enable:vdd d in
  let d = Signal.reg reg_spec ~enable:vdd d in
  w <== d;
  test [ d ];
  [%expect {|
    (Ok ()) |}]
;;

(* same as above, but a different arrangement of the adder *)
let%expect_test "loop through 2 registers" =
  let reg_spec = Reg_spec.create () ~clock in
  let a = input "a" 2 in
  let w = wire 2 in
  let d = Signal.reg reg_spec ~enable:vdd w in
  let d = Signal.reg reg_spec ~enable:vdd d in
  w <== a +: d;
  test [ d ];
  [%expect {|
    (Ok ()) |}]
;;

let%expect_test "combinational loop before a register" =
  let reg_spec = Reg_spec.create () ~clock in
  let a = input "a" 2 in
  let w = wire 2 in
  let b = a &: w in
  w <== b;
  let c = reg reg_spec ~enable:vdd b in
  test [ c ];
  [%expect
    {|
    (Error (
      "combinational loop" (through_signal (and (width 2) (arguments (a wire)))))) |}]
;;

let%expect_test "combinational loop between registers" =
  let reg_spec = Reg_spec.create () ~clock in
  let a = reg reg_spec ~enable:vdd (input "a" 2) in
  let w = wire 2 in
  let b = a &: w in
  w <== b;
  let c = reg reg_spec ~enable:vdd b in
  test [ c ];
  [%expect
    {|
    (Error (
      "combinational loop" (
        through_signal (and (width 2) (arguments (register wire)))))) |}]
;;

let%expect_test "combinational loop inside register loop" =
  let reg_spec = Reg_spec.create () ~clock in
  let a =
    reg_fb reg_spec ~enable:vdd ~w:2 (fun d ->
      let w = wire 2 -- "wire_in_loop" in
      let e = d +: w in
      w <== w;
      e)
  in
  test [ a ];
  [%expect
    {|
    (Error (
      "combinational loop" (
        through_signal (
          wire
          (names (wire_in_loop))
          (width   2)
          (data_in wire_in_loop))))) |}]
;;

let%expect_test "looping memory" =
  let w = wire 4 in
  let a =
    memory
      2
      ~write_port:
        { write_clock = clock
        ; write_enable = bit w 0
        ; write_address = bit w 1
        ; write_data = uresize (bit w 3) 4
        }
      ~read_address:(bit w 2)
  in
  w <== a;
  test [ a ];
  [%expect {| (Ok ()) |}]
;;

let%expect_test "looping instantiation" =
  let w = wire 1 in
  let x = Instantiation.create () ~name:"foo" ~inputs:[ "a", w ] ~outputs:[ "b", 1 ] in
  let b = x#o "b" in
  w <== b;
  test [ b ];
  [%expect {| (Ok ()) |}]
;;
