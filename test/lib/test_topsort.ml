open! Import

let%expect_test "signed resize" =
  let a = Signal.input "a" 4 in
  let b = Signal.sresize a 8 in
  let result = Signal_graph.topological_sort (Signal_graph.create [ b ]) in
  print_s [%sexp (result : Signal.t list)];
  [%expect
    {|
    (empty
      (wire
        (names (a))
        (width   4)
        (data_in empty))
      (select (width 1) (range (3 3)) (data_in a))
      (cat (width 2) (arguments (select select)))
      (cat (width 4) (arguments (cat    cat)))
      (cat (width 8) (arguments (cat    a)))) |}]
;;

let%expect_test "a + a" =
  let a = Signal.input "a" 4 in
  let b = Signal.(a +: a) in
  let result = Signal_graph.topological_sort (Signal_graph.create [ b ]) in
  print_s [%sexp (result : Signal.t list)];
  [%expect
    {|
    (empty
      (wire
        (names (a))
        (width   4)
        (data_in empty))
      (add (width 4) (arguments (a a)))) |}]
;;

(* Simplest possible circuit. *)
let%expect_test "input -> output" =
  let a = Signal.input "a" 4 in
  let b = Signal.output "b" a in
  let result = Signal_graph.topological_sort (Signal_graph.create [ b ]) in
  print_s [%sexp (result : Signal.t list)];
  [%expect
    {|
    (empty
      (wire
        (names (a))
        (width   4)
        (data_in empty))
      (wire
        (names (b))
        (width   4)
        (data_in a))) |}]
;;

let reg_spec = Reg_spec.create () ~clock ~clear

(* You require custom [deps] to express the fact that you may loop through certain
   nodes. *)
let%expect_test "reg loop (standard deps)" =
  let b = Signal.reg_fb reg_spec ~enable:Signal.vdd ~w:1 Signal.(fun d -> d +:. 1) in
  require_does_raise [%here] (fun () ->
    Signal_graph.topological_sort (Signal_graph.create [ b ]));
  [%expect {|
    ("Topological_sort.sort encountered cycle" (add wire register)) |}]
;;

(* Plug in the correct [deps] *)
let deps (s : Signal.t) =
  match s with
  | Mem { memory = m; _ } -> [ m.mem_read_address ]
  | Reg _ -> []
  | _ -> Signal.deps s
;;

let%expect_test "reg loop" =
  let b = Signal.reg_fb reg_spec ~enable:Signal.vdd ~w:1 Signal.(fun d -> d +:. 1) in
  let result = Signal_graph.topological_sort ~deps (Signal_graph.create [ b ]) in
  print_s [%sexp (result : Signal.t list)];
  [%expect
    {|
    (empty
      (wire
        (names (clock))
        (width   1)
        (data_in empty))
      (wire
        (names (clear))
        (width   1)
        (data_in empty))
      (register
        (width 1)
        ((clock       clock)
         (clock_edge  Rising)
         (clear       clear)
         (clear_level High)
         (clear_to    0b0)
         (enable      0b1))
        (data_in wire))
      (const
        (width 1)
        (value 0b1))
      (add (width 1) (arguments (register 0b1)))
      (wire
        (width   1)
        (data_in add))
      (const
        (names (vdd))
        (width 1)
        (value 0b1))
      (const (width 1) (value 0b0))
      (const (width 1) (value 0b0))) |}]
;;

let%expect_test "mem loop" =
  let w = Signal.wire 1 in
  let q =
    Signal.memory
      2
      ~write_port:
        { write_clock = clock; write_enable = w; write_address = w; write_data = w }
      ~read_address:Signal.vdd
  in
  Signal.(w <== q);
  let result = Signal_graph.topological_sort ~deps (Signal_graph.create [ q ]) in
  print_s [%sexp (result : Signal.t list)];
  [%expect
    {|
    (empty
      (const
        (names (vdd))
        (width 1)
        (value 0b1))
      (wire
        (names (clock))
        (width   1)
        (data_in empty))
      (memory
        (width 1)
        ((clock      clock)
         (clock_edge Rising)
         (enable     wire))
        ((size          2)
         (write_address wire)
         (read_address  0b1)
         (write_enable  wire))
        (data_in wire))
      (wire  (width 1) (data_in memory))
      (const (width 1) (value   0b0))
      (const (width 1) (value   0b0))) |}]
;;

(* This a combinational loop.  The read address is not synchronously read. *)
let%expect_test "mem loop, including read address, which isn't allowed" =
  let w = Signal.wire 1 in
  let q =
    Signal.memory
      2
      ~write_port:
        { write_clock = clock; write_enable = w; write_address = w; write_data = w }
      ~read_address:w
  in
  Signal.(w <== q);
  require_does_raise [%here] (fun () ->
    Signal_graph.topological_sort ~deps (Signal_graph.create [ q ]));
  [%expect {|
    ("Topological_sort.sort encountered cycle" (memory wire)) |}]
;;

(* Instantiation - the only types of instantiation possible in a simulation are
   [Combinational_op]s which don't allow cycles. *)
let%expect_test "Instantiation loop - not allowed." =
  let w = Signal.wire 1 in
  let inst =
    Instantiation.create () ~name:"foo" ~inputs:[ "a", w ] ~outputs:[ "b", 1 ]
  in
  Signal.(w <== inst#o "b");
  require_does_raise [%here] (fun () ->
    Signal_graph.topological_sort ~deps (Signal_graph.create [ w ]));
  [%expect {|
    ("Topological_sort.sort encountered cycle" (instantiation wire)) |}]
;;
