open! Import

let%expect_test "Uid hashes are equal between ocaml and javascript" =
  let `New new_id, _ = Signal.Uid.generator () in
  let id = new_id () in
  print_s [%message (id : Signal.Uid.t) (Signal.Uid.hash id : int)];
  [%expect {|
    ((id                   1)
     ("Signal.Uid.hash id" 746625832))
    |}];
  let id = new_id () in
  print_s [%message (id : Signal.Uid.t) (Signal.Uid.hash id : int)];
  [%expect {|
    ((id                   2)
     ("Signal.Uid.hash id" 391307823))
    |}];
  let id = new_id () in
  print_s [%message (id : Signal.Uid.t) (Signal.Uid.hash id : int)];
  [%expect {|
    ((id                   3)
     ("Signal.Uid.hash id" 834235799))
    |}]
;;

let topsort g =
  Signal_graph.topological_sort ~deps:(module Signal.Type.Deps) (Signal_graph.create g)
;;

let sexp_of_signal = Signal.Type.sexp_of_signal_recursive ~show_uids:false ~depth:0
let sexp_of_topsort = [%sexp_of: (signal list, signal list) Result.t]

let%expect_test "signed resize" =
  let a = Signal.input "a" 4 in
  let b = Signal.sresize a 8 in
  let result = topsort [ b ] in
  print_s [%sexp (result : topsort)];
  [%expect {| (Ok (empty a select cat cat cat)) |}]
;;

let%expect_test "a + a" =
  let a = Signal.input "a" 4 in
  let b = Signal.(a +: a) in
  let result = topsort [ b ] in
  print_s [%sexp (result : topsort)];
  [%expect {| (Ok (empty a add)) |}]
;;

(* Simplest possible circuit. *)
let%expect_test "input -> output" =
  let a = Signal.input "a" 4 in
  let b = Signal.output "b" a in
  let result = topsort [ b ] in
  print_s [%sexp (result : topsort)];
  [%expect {| (Ok (empty a b)) |}]
;;

let reg_spec = Reg_spec.create () ~clock ~clear

(* You require custom [deps] to express the fact that you may loop through certain
   nodes. *)
let%expect_test "reg loop (standard deps)" =
  let b = Signal.reg_fb reg_spec ~width:1 ~f:Signal.(fun d -> d +:. 1) in
  let loop = topsort [ b ] in
  print_s [%message (loop : topsort)];
  [%expect {| (loop (Error (register add wire))) |}]
;;

(* Plug in the correct [deps] *)
let topsort g =
  Signal_graph.topological_sort
    ~deps:(module Signal_graph.Deps_for_simulation_scheduling)
    (Signal_graph.create g)
;;

let%expect_test "reg loop" =
  let b =
    Signal.reg_fb reg_spec ~enable:Signal.vdd ~width:1 ~f:Signal.(fun d -> d +:. 1)
  in
  let result = topsort [ b ] in
  print_s [%sexp (result : topsort)];
  [%expect {| (Ok (register 0b1 add wire empty clock 0b0 clear 0b0 0b1)) |}]
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
  let result = topsort [ q ] in
  print_s [%sexp (result : topsort)];
  [%expect {| (Ok (0b1 memory_read_port multiport_memory empty clock wire)) |}]
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
  let result = topsort [ q ] in
  print_s [%sexp (result : topsort)];
  [%expect {| (Error (wire memory_read_port)) |}]
;;

(* Instantiation - the only types of instantiation possible in a simulation are
   [Combinational_op]s which don't allow cycles. *)
let%expect_test "Instantiation loop - not allowed." =
  let w = Signal.wire 1 in
  let inst = Instantiation.create () ~name:"foo" ~inputs:[ "a", w ] ~outputs:[ "b", 1 ] in
  Signal.(w <== Map.find_exn inst "b");
  let result = topsort [ w ] in
  print_s [%sexp (result : topsort)];
  [%expect {| (Error (instantiation wire wire)) |}]
;;
