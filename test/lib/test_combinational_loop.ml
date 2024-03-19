open! Import
open Signal

let test o =
  print_s
    [%sexp
      (Signal_graph.create o |> Signal_graph.detect_combinational_loops : unit Or_error.t)]
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
      "Combinational loop" ((
        wire
        (width   1)
        (data_in wire)))))
    |}]
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
      "Combinational loop" (
        (wire
          (width   2)
          (data_in add))
        (add (width 2) (arguments (a wire))))))
    |}]
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
      "Combinational loop" (
        (xor (width 2) (arguments (xor and)))
        (wire
          (width   2)
          (data_in xor))
        (add (width 2) (arguments (wire a)))
        (or  (width 2) (arguments (add  b)))
        (and (width 2) (arguments (or   c)))
        (xor (width 2) (arguments (and  mux))))))
    |}];
  test [ e ];
  [%expect
    {|
    (Error (
      "Combinational loop" (
        (add (width 2) (arguments (and c)))
        (mux
          (width  2)
          (select select)
          (data (or add)))
        (xor (width 2) (arguments (and mux)))
        (xor (width 2) (arguments (xor and)))
        (wire
          (width   2)
          (data_in xor))
        (add (width 2) (arguments (wire a)))
        (or  (width 2) (arguments (add  b)))
        (and (width 2) (arguments (or   c))))))
    |}];
  test [ f ];
  [%expect
    {|
    (Error (
      "Combinational loop" (
        (and (width 2) (arguments (or  c)))
        (and (width 2) (arguments (and mux)))
        (xor (width 2) (arguments (xor and)))
        (wire
          (width   2)
          (data_in xor))
        (add (width 2) (arguments (wire a)))
        (or  (width 2) (arguments (add  b))))))
    |}];
  test [ g ];
  [%expect
    {|
    (Error (
      "Combinational loop" (
        (xor (width 2) (arguments (xor and)))
        (wire
          (width   2)
          (data_in xor))
        (add (width 2) (arguments (wire a)))
        (or  (width 2) (arguments (add  b)))
        (and (width 2) (arguments (or   c)))
        (xor (width 2) (arguments (and  mux))))))
    |}]
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
      "Combinational loop" (
        (wire
          (width   2)
          (data_in add))
        (add (width 2) (arguments (a wire))))))
    |}]
;;

let%expect_test "loop through register" =
  let a =
    Signal.reg_fb (Reg_spec.create () ~clock) ~enable:vdd ~width:2 ~f:(fun d -> d +:. 1)
  in
  test [ a ];
  [%expect {| (Ok ()) |}]
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
  [%expect {| (Ok ()) |}]
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
  [%expect {| (Ok ()) |}]
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
      "Combinational loop" (
        (wire
          (width   2)
          (data_in and))
        (and (width 2) (arguments (a wire))))))
    |}]
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
      "Combinational loop" (
        (wire
          (width   2)
          (data_in and))
        (and (width 2) (arguments (register wire))))))
    |}]
;;

let%expect_test "combinational loop inside register loop" =
  let reg_spec = Reg_spec.create () ~clock in
  let a =
    reg_fb reg_spec ~enable:vdd ~width:2 ~f:(fun d ->
      let w = wire 2 -- "wire_in_loop" in
      let e = d +: w in
      w <== w;
      e)
  in
  test [ a ];
  [%expect
    {|
    (Error (
      "Combinational loop" ((
        wire
        (names (wire_in_loop))
        (width   2)
        (data_in wire_in_loop)))))
    |}]
;;

let%expect_test "looping memory - q to read_address" =
  let write_enable = wire 1 in
  let write_address = wire 1 in
  let write_data = wire 1 in
  let read_address = wire 1 in
  let q =
    memory
      2
      ~write_port:{ write_clock = clock; write_enable; write_address; write_data }
      ~read_address
  in
  read_address <== q.:(0);
  test [ q ];
  [%expect
    {|
    (Error (
      "Combinational loop" (
        (wire
          (width   1)
          (data_in memory_read_port))
        (memory_read_port
          (width 1)
          ((memory         multiport_memory)
           (read_addresses wire))))))
    |}]
;;

let%expect_test "no loop in memory - q to write port" =
  (* write address *)
  let write_enable = wire 1 in
  let write_address = wire 1 in
  let write_data = wire 1 in
  let read_address = wire 1 in
  let q =
    memory
      2
      ~write_port:{ write_clock = clock; write_enable; write_address; write_data }
      ~read_address
  in
  write_address <== q.:(0);
  test [ q ];
  (* write enable *)
  [%expect {| (Ok ()) |}];
  let write_enable = wire 1 in
  let write_address = wire 1 in
  let write_data = wire 1 in
  let read_address = wire 1 in
  let q =
    memory
      2
      ~write_port:{ write_clock = clock; write_enable; write_address; write_data }
      ~read_address
  in
  write_enable <== q.:(0);
  test [ q ];
  [%expect {| (Ok ()) |}];
  (* write data *)
  let write_enable = wire 1 in
  let write_address = wire 1 in
  let write_data = wire 1 in
  let read_address = wire 1 in
  let q =
    memory
      2
      ~write_port:{ write_clock = clock; write_enable; write_address; write_data }
      ~read_address
  in
  write_data <== q.:(0);
  test [ q ];
  [%expect {| (Ok ()) |}]
;;

let%expect_test "looping instantiation" =
  let w = wire 1 in
  let x = Instantiation.create () ~name:"foo" ~inputs:[ "a", w ] ~outputs:[ "b", 1 ] in
  let b = Map.find_exn x "b" in
  w <== b;
  test [ b ];
  [%expect {| (Ok ()) |}]
;;
