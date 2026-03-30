open! Import
open Signal

let reg_spec = Reg_spec.create () ~clock ~clear

let%expect_test "combinational logic with no regs" =
  (*=
       a (from)
       |
       + (b)
       |
       & (c)
       |
       | (d, to_)
  *)
  let a = input "a" 4 in
  let b = a +: a in
  let c = b &: b in
  let d = c |: c in
  let count = Signal_graph.count_regs_between ~from:a ~to_:d in
  print_s [%message (count : int)];
  [%expect {| (count 0) |}]
;;

let%expect_test "reg to reg chain - from reg not counted, to_ reg counted" =
  (*=
       a
       |
      [r1] (from) <-- not counted
       |
      [r2] (to_)  <-- counted
  *)
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec r1 in
  let count = Signal_graph.count_regs_between ~from:r1 ~to_:r2 in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "three regs in a pipeline" =
  (*=
       a (from) <-- not counted
       |
      [r1] <-- counted
       |
      [r2] <-- counted
       |
      [r3] (to_) <-- counted
  *)
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec r1 in
  let r3 = reg reg_spec r2 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r3 in
  print_s [%message (count : int)];
  [%expect {| (count 3) |}]
;;

let%expect_test "multiple paths with same reg count" =
  (*=
         a (from)
        / \
      [r1] [r2]  <-- both paths have 1 reg
        \   /
          +
          |
        sum (to_)
  *)
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec a in
  let sum = r1 +: r2 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:sum in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "multiple paths with different reg counts - raises" =
  (*=
         a (from)
        / \
      [r1] |   <-- left path: 1 reg, right path: 0 regs
        \ |
         \|
          +
          |
        sum (to_)
  *)
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let sum = r1 +: a in
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:sum);
  [%expect
    {|
    (raised (
      "Multiple paths with different register counts"
      (from_signal (wire (names (a)) (width 4)))
      (to_signal (add (width 4) (arguments (register a))))
      (counts (0 1))))
    |}]
;;

let%expect_test "no path between signals - raises" =
  (*=
       a        b (from)
       |        |
       +      (no connection to c)
       |
       c (to_)
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let c = a +: a in
  show_raise (fun () -> Signal_graph.count_regs_between ~from:b ~to_:c);
  [%expect
    {|
    (raised (
      "No path found between signals"
      (from_signal (wire (names (b)) (width 4)))
      (to_signal (add (width 4) (arguments (a a))))))
    |}]
;;

let%expect_test "from equals to - zero regs" =
  let a = input "a" 4 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:a in
  print_s [%message (count : int)];
  [%expect {| (count 0) |}]
;;

let%expect_test "reg with combinational logic before and after" =
  (*=
       a (from)
       |
       + (b)
       |
      [r] <-- counted
       |
       & (c, to_)
  *)
  let a = input "a" 4 in
  let b = a +: a in
  let r = reg reg_spec b in
  let c = r &: r in
  let count = Signal_graph.count_regs_between ~from:a ~to_:c in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "mux with balanced reg counts" =
  (*=
            a (from)
           / \
         [r1] [r2]  <-- both paths have 1 reg
           \   /
        sel--mux
             |
             m (to_)
  *)
  let sel = input "sel" 1 in
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec a in
  let m = mux2 sel r1 r2 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:m in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "mux with unbalanced reg counts - raises" =
  (*=
            a (from)
           / \
         [r1] |   <-- left: 1 reg, right: 0 regs
           \ |
        sel--mux
             |
             m (to_)
  *)
  let sel = input "sel" 1 in
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let m = mux2 sel r1 a in
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:m);
  [%expect
    {|
    (raised (
      "Multiple paths with different register counts"
      (from_signal (wire (names (a)) (width 4)))
      (to_signal (
        mux
        (width  4)
        (select sel)
        (data (a register))))
      (counts (1 0))))
    |}]
;;

let%expect_test "complex diamond with consistent paths" =
  (*=
            a (from)
          / | \
       [r1][r2][r3]  <-- all 3 paths have 1 reg
          \ | /
            +
            |
          sum (to_)
  *)
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec a in
  let r3 = reg reg_spec a in
  let sum = r1 +: r2 +: r3 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:sum in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Cat - unbalanced concatenation raises" =
  (*=
       a (from)
      / \
    [r]  |
      \  |
       cat
        |
        b (to_)
  *)
  let a = input "a" 4 in
  let r = reg reg_spec a in
  let b = r @: a in
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:b);
  [%expect
    {|
    (raised (
      "Multiple paths with different register counts"
      (from_signal (wire (names (a)) (width 4)))
      (to_signal (cat (width 8) (arguments (register a))))
      (counts (0 1))))
    |}]
;;

let%expect_test "Select - bit selection" =
  (*=
       a (from)
       |
      [r]
       |
      sel [2:1]
       |
       b (to_)
  *)
  let a = input "a" 4 in
  let r = reg reg_spec a in
  let b = sel_bottom r ~width:2 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:b in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Memory - async read, no regs in read path" =
  (*=
       a (from) --- write_data
       |
      [mem] (async read)
       |
       q (to_)
  *)
  let a = input "a" 4 in
  let addr = input "addr" 2 in
  let we = input "we" 1 in
  let q =
    memory
      4
      ~write_port:
        { write_clock = clock; write_enable = we; write_address = addr; write_data = a }
      ~read_address:addr
  in
  let count = Signal_graph.count_regs_between ~from:a ~to_:q in
  print_s [%message (count : int)];
  [%expect {| (count 0) |}]
;;

let%expect_test "Memory - with output register" =
  (*=
       a (from) --- write_data
       |
      [mem]
       |
      [r] <-- output register
       |
       q (to_)
  *)
  let a = input "a" 4 in
  let addr = input "addr" 2 in
  let we = input "we" 1 in
  let mem_out =
    memory
      4
      ~write_port:
        { write_clock = clock; write_enable = we; write_address = addr; write_data = a }
      ~read_address:addr
  in
  let q = reg reg_spec mem_out in
  let count = Signal_graph.count_regs_between ~from:a ~to_:q in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Instantiation - raises" =
  (*=
       a (from)
       |
      [inst] (instantiations not supported)
       |
       b (to_)
  *)
  let a = input "a" 4 in
  let inst =
    Instantiation.create () ~name:"passthrough" ~inputs:[ "i", a ] ~outputs:[ "o", 4 ]
  in
  let b = Instantiation.output inst "o" in
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:b);
  [%expect
    {|
    (raised (
      "Instantiation found on path between signals - not supported"
      (circuit_name passthrough)))
    |}]
;;

let%expect_test "Cases - balanced paths" =
  (*=
         a (from)
        /|\
      [r][r][r]  <-- all cases have 1 reg
        \|/
       cases
         |
         b (to_)
  *)
  let sel = input "sel" 2 in
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec a in
  let r3 = reg reg_spec a in
  let b =
    Signal.cases
      sel
      ~default:r3
      [ of_int_trunc ~width:2 0, r1; of_int_trunc ~width:2 1, r2 ]
  in
  let count = Signal_graph.count_regs_between ~from:a ~to_:b in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Cases - unbalanced paths raises" =
  (*=
         a (from)
        /|\
      [r] | [r]  <-- middle case has no reg
        \|/
       cases
         |
         b (to_)
  *)
  let sel = input "sel" 2 in
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec a in
  let b =
    Signal.cases
      sel
      ~default:r2
      [ of_int_trunc ~width:2 0, r1; of_int_trunc ~width:2 1, a ]
  in
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:b);
  [%expect
    {|
    (raised (
      "Multiple paths with different register counts"
      (from_signal (wire (names (a)) (width 4)))
      (to_signal (
        cases
        (width  4)
        (select sel)
        (cases (
          (0b00 register)
          (0b01 a)))
        (default register)))
      (counts (1 0 1))))
    |}]
;;

(* Tests for wires assigned before/after counting *)

let%expect_test "Wire assigned after counting - raises (no path at count time)" =
  (*=
       a (from)
       |
      [r]
       |
       w (wire, assigned AFTER counting attempted)
       |
       b (to_)
  *)
  let a = input "a" 4 in
  let r = reg reg_spec a in
  let w = wire 4 in
  (* Try to count BEFORE assigning the wire *)
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:w);
  [%expect
    {|
    (raised (
      "No path found between signals"
      (from_signal (wire (names (a)) (width 4)))
      (to_signal (wire (width 4)))))
    |}];
  (* Now assign the wire *)
  w <-- r;
  (* Now counting works *)
  let count = Signal_graph.count_regs_between ~from:a ~to_:w in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Wire in middle of path - assigned before counting" =
  (*=
       a (from)
       |
      [r1]
       |
       w (wire)
       |
      [r2]
       |
       b (to_)
  *)
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let w = wire 4 in
  w <-- r1;
  let r2 = reg reg_spec w in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r2 in
  print_s [%message (count : int)];
  [%expect {| (count 2) |}]
;;

let%expect_test "Wire in middle of path - assigned after creating downstream logic" =
  (*=
       a (from)
       |
      [r1]
       |
       w (wire, assigned after r2 is created)
       |
      [r2]
       |
       b (to_)
  *)
  let a = input "a" 4 in
  let r1 = reg reg_spec a in
  let w = wire 4 in
  (* Create downstream logic BEFORE assigning wire *)
  let r2 = reg reg_spec w in
  (* Counting fails because wire not yet assigned *)
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:r2);
  [%expect
    {|
    (raised (
      "No path found between signals"
      (from_signal (wire (names (a)) (width 4)))
      (to_signal (
        register
        (width 4)
        ((clock      clock)
         (clock_edge Rising)
         (clear      clear)
         (clear_to   0b0000))
        (data_in wire)))))
    |}];
  (* Now assign the wire *)
  w <-- r1;
  (* Now counting works *)
  let count = Signal_graph.count_regs_between ~from:a ~to_:r2 in
  print_s [%message (count : int)];
  [%expect {| (count 2) |}]
;;

(* Tests for circuits with unrelated inputs and outputs *)

let%expect_test "Unrelated input - not on path" =
  (*=
       a (from)      b (unrelated input)
       |             |
      [r1]          [r2]
       |             |
      [r3]          [r4]
       |             |
       c (to_)       d (unrelated output)

     Only counting from a to c, ignoring b entirely.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec b in
  let r3 = reg reg_spec r1 in
  let _r4 = reg reg_spec r2 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r3 in
  print_s [%message (count : int)];
  [%expect {| (count 2) |}]
;;

let%expect_test "Multiple unrelated inputs feeding into separate outputs" =
  (*=
       a (from)    b         c (unrelated inputs)
       |           |         |
      [r1]        [r2]      [r3]
       |           |         |
       d (to_)     e         f (unrelated outputs)

     Only counting from a to d, ignoring b->e and c->f paths.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let c = input "c" 4 in
  let r1 = reg reg_spec a in
  let _r2 = reg reg_spec b in
  let _r3 = reg reg_spec c in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r1 in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Unrelated input merged after measurement point" =
  (*=
       a (from)        b (unrelated input)
       |               |
      [r1]            [r2]
       |               |
       c (to_)         |
       |               |
       +---------------+
       |
       d (combined output, not part of measurement)

     Counting from a to c, even though c is later combined with b's path.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec b in
  let _d = r1 +: r2 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r1 in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Unrelated input joins path after from but before to_" =
  (*=
       a (from)        b (unrelated input)
       |               |
      [r1]            [r2]
       |               |
       +-------+-------+
               |
              [r3]
               |
               c (to_)

     From a to c has 2 regs. From b to c also has 2 regs.
     The paths merge but have consistent reg counts.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec b in
  let sum = r1 +: r2 in
  let r3 = reg reg_spec sum in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r3 in
  print_s [%message (count : int)];
  [%expect {| (count 2) |}]
;;

let%expect_test "Complex circuit with many unrelated paths" =
  (*=
       a (from)    b    c    d (unrelated inputs)
       |           |    |    |
      [r1]        [r2] [r3] [r4]
       |           |    |    |
      [r5]         |    |    |
       |           |    |    |
       e (to_)     f    g    h (unrelated outputs)

     Only measuring a->e (2 regs), ignoring all other paths.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let c = input "c" 4 in
  let d = input "d" 4 in
  let r1 = reg reg_spec a in
  let _r2 = reg reg_spec b in
  let _r3 = reg reg_spec c in
  let _r4 = reg reg_spec d in
  let r5 = reg reg_spec r1 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r5 in
  print_s [%message (count : int)];
  [%expect {| (count 2) |}]
;;

let%expect_test "Unrelated input with different pipeline depth" =
  (*=
       a (from)              b (unrelated input)
       |                     |
      [r1]                  [r2]
       |                     |
       c (to_)              [r3]
                             |
                            [r4]
                             |
                             d (unrelated output)

     Path a->c has 1 reg. Unrelated path b->d has 3 regs.
     Only counting a->c.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec b in
  let r3 = reg reg_spec r2 in
  let _r4 = reg reg_spec r3 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r1 in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Shared combinational logic with unrelated input" =
  (*=
       a (from)    b (unrelated input)
       |           |
       +-----------+
       |
       + (shared adder)
       |
      [r1]
       |
       c (to_)

     Both a and b feed into the same adder, but we only count from a.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let sum = a +: b in
  let r1 = reg reg_spec sum in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r1 in
  print_s [%message (count : int)];
  [%expect {| (count 1) |}]
;;

let%expect_test "Mux with unrelated selector input" =
  (*=
       a (from)    sel (unrelated input, not on data path)
       |            |
      [r1]         |
       |           |
      [r2]         |
       |   +-------+
       |   |
      mux--+
       |
       b (to_)

     Selector is not on the data path from a, so we only count regs from a.
  *)
  let a = input "a" 4 in
  let sel = input "sel" 1 in
  let r1 = reg reg_spec a in
  let r2 = reg reg_spec r1 in
  let b = mux2 sel r2 r2 in
  let count = Signal_graph.count_regs_between ~from:a ~to_:b in
  print_s [%message (count : int)];
  [%expect {| (count 2) |}]
;;

(* Tests for reversed from/to_ arguments *)

let%expect_test "Reversed: reg in middle" =
  (*=
       a
       |
       + (b)
       |
      [r]
       |
       & (c)

     Forward (a -> c): 1 reg
     Forward (b -> c): 1 reg
     Forward (r -> c): 0 regs (r is from, not counted)
     Reversed: no path in all cases
  *)
  let a = input "a" 4 in
  let b = a +: a in
  let r = reg reg_spec b in
  let c = r &: r in
  print_s
    [%message "a to c" ~count:(Signal_graph.count_regs_between ~from:a ~to_:c : int)];
  [%expect {| ("a to c" (count 1)) |}];
  print_s
    [%message "b to c" ~count:(Signal_graph.count_regs_between ~from:b ~to_:c : int)];
  [%expect {| ("b to c" (count 1)) |}];
  print_s
    [%message "r to c" ~count:(Signal_graph.count_regs_between ~from:r ~to_:c : int)];
  [%expect {| ("r to c" (count 0)) |}];
  show_raise (fun () -> Signal_graph.count_regs_between ~from:c ~to_:a);
  [%expect
    {|
    (raised (
      "No path found between signals"
      (from_signal (and (width 4) (arguments (register register))))
      (to_signal (wire (names (a)) (width 4)))))
    |}]
;;

(* Tests for circuits with loops (feedback through registers) *)

let%expect_test "Loop: simple feedback register (accumulator pattern)" =
  (*=
       a (input)
       |
       +------ feedback wire
       |           |
       +  <--------+
       |
      [r] -----> output (also feeds back)
       |
       b (to_)

     This is an accumulator: r = reg(r + a)
     Counting from a to r: 1 reg
     Counting from a to the sum: 1 reg (through the feedback path via r)
  *)
  let a = input "a" 4 in
  let feedback = wire 4 in
  let sum = a +: feedback in
  let r = reg reg_spec sum in
  feedback <-- r;
  (* Forward path from input to register output *)
  let count = Signal_graph.count_regs_between ~from:a ~to_:r in
  print_s [%message "a to r" (count : int)];
  [%expect {| ("a to r" (count 1)) |}];
  (* Path from input to the sum (before the register) *)
  let count = Signal_graph.count_regs_between ~from:a ~to_:sum in
  print_s [%message "a to sum" (count : int)];
  [%expect {| ("a to sum" (count 0)) |}]
;;

let%expect_test "Loop: feedback with multiple regs in loop" =
  (*=
       a (input)
       |
       +------ feedback wire
       |           |
       +  <--------+
       |
      [r1]
       |
      [r2] -----> feeds back
       |
       b (to_)

     Two registers in the feedback path.
     Counting from a to r2: 2 regs
  *)
  let a = input "a" 4 in
  let feedback = wire 4 in
  let sum = a +: feedback in
  let r1 = reg reg_spec sum in
  let r2 = reg reg_spec r1 in
  feedback <-- r2;
  let count = Signal_graph.count_regs_between ~from:a ~to_:r2 in
  print_s [%message "a to r2" (count : int)];
  [%expect {| ("a to r2" (count 2)) |}];
  let count = Signal_graph.count_regs_between ~from:a ~to_:r1 in
  print_s [%message "a to r1" (count : int)];
  [%expect {| ("a to r1" (count 1)) |}];
  let count = Signal_graph.count_regs_between ~from:r2 ~to_:r1 in
  print_s [%message "r2 to r1" (count : int)];
  [%expect {| ("r2 to r1" (count 1)) |}];
  let count = Signal_graph.count_regs_between ~from:r2 ~to_:r2 in
  print_s [%message "r2 to r2" (count : int)];
  [%expect {| ("r2 to r2" (count 0)) |}]
;;

let%expect_test "Loop: two independent feedback loops" =
  (*=
       a           b
       |           |
       +--fb1      +--fb2
       |   |       |   |
       +<--+       +<--+
       |           |
      [r1]        [r2]
       |           |
       c           d

     Two separate accumulators. Counting in each should be independent.
  *)
  let a = input "a" 4 in
  let b = input "b" 4 in
  let fb1 = wire 4 in
  let fb2 = wire 4 in
  let sum1 = a +: fb1 in
  let sum2 = b +: fb2 in
  let r1 = reg reg_spec sum1 in
  let r2 = reg reg_spec sum2 in
  fb1 <-- r1;
  fb2 <-- r2;
  print_s
    [%message "a to r1" ~count:(Signal_graph.count_regs_between ~from:a ~to_:r1 : int)];
  [%expect {| ("a to r1" (count 1)) |}];
  print_s
    [%message "b to r2" ~count:(Signal_graph.count_regs_between ~from:b ~to_:r2 : int)];
  [%expect {| ("b to r2" (count 1)) |}];
  (* No path between the two loops *)
  show_raise (fun () -> Signal_graph.count_regs_between ~from:a ~to_:r2);
  [%expect
    {|
    (raised (
      "No path found between signals"
      (from_signal (wire (names (a)) (width 4)))
      (to_signal (
        register
        (width 4)
        ((clock      clock)
         (clock_edge Rising)
         (clear      clear)
         (clear_to   0b0000))
        (data_in add)))))
    |}]
;;

let%expect_test "Loop: feedback loop with mux (state machine pattern)" =
  (*=
       sel
        |
       a|   feedback
        |      |
       mux <---+
        |
       [r] -----> feeds back
        |
        b

     State machine pattern: next_state = mux(sel, a, current_state)
  *)
  let sel = input "sel" 1 in
  let a = input "a" 4 in
  let feedback = wire 4 in
  let next_state = mux2 sel a feedback in
  let r = reg reg_spec next_state in
  feedback <-- r;
  (* From a to r: 1 reg (through the mux when sel=1) *)
  let count = Signal_graph.count_regs_between ~from:a ~to_:r in
  print_s [%message "a to r" (count : int)];
  [%expect {| ("a to r" (count 1)) |}];
  (* From sel to r: 0 regs (sel is just the mux selector, combinational to the mux output) *)
  let count = Signal_graph.count_regs_between ~from:sel ~to_:r in
  print_s [%message "sel to r" (count : int)];
  [%expect {| ("sel to r" (count 1)) |}]
;;

let%expect_test "Loop: nested feedback loops" =
  let a = input "a" 4 in
  let fb_outer = wire 4 in
  let fb_inner = wire 4 in
  let sum_outer = a +: fb_outer in
  let r1 = reg reg_spec sum_outer in
  let sum_inner = r1 +: fb_inner in
  let r2 = reg reg_spec sum_inner in
  fb_outer <-- r1;
  fb_inner <-- r2;
  (* r1 = r1 (fb_outer) + a *)
  (* r2 = r1 + r2 (fb_inner) *)
  print_s
    [%message "a to r1" ~count:(Signal_graph.count_regs_between ~from:a ~to_:r1 : int)];
  [%expect {| ("a to r1" (count 1)) |}];
  print_s
    [%message "a to r2" ~count:(Signal_graph.count_regs_between ~from:a ~to_:r2 : int)];
  [%expect {| ("a to r2" (count 2)) |}];
  print_s
    [%message "r1 to r2" ~count:(Signal_graph.count_regs_between ~from:r1 ~to_:r2 : int)];
  [%expect {| ("r1 to r2" (count 1)) |}]
;;

let%expect_test "Loop: enabled register with feedback" =
  (*=
       a      enable
       |        |
       +--fb    |
       |  |     |
       + <+     |
       |        |
      [r] <-----+ (enable controls register)
       |
       b

     reg_fb pattern with enable
  *)
  let a = input "a" 4 in
  let enable = input "enable" 1 in
  let r = reg_fb reg_spec ~width:4 ~enable ~f:(fun feedback -> a +: feedback) in
  let count = Signal_graph.count_regs_between ~from:a ~to_:r in
  print_s [%message "a to r" (count : int)];
  [%expect {| ("a to r" (count 1)) |}];
  let count = Signal_graph.count_regs_between ~from:enable ~to_:r in
  print_s [%message "enable to r" (count : int)];
  [%expect {| ("enable to r" (count 1)) |}]
;;
