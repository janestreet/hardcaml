(* Test generation of hierarchical circuits. *)
open! Import
open Signal

(* [inner] *)
let inner ~cause_exn ~db ~share =
  let a = input "a" 1 in
  let b = output (if cause_exn then "a" else "b") a in
  let circ = Circuit.create_exn ~name:"inner" [ b ] in
  let name = Circuit_database.insert ~share db circ in
  name
;;

(* [middle] *)
let middle ~cause_exn ~db ~share =
  let inner_name1 = inner ~cause_exn ~db ~share in
  let inner_name2 = inner ~cause_exn ~db ~share in
  (* create 2 instantiations of [inner] so we can demonstrate the effect of sharing. *)
  let a = input "a" 1 in
  let inst =
    Instantiation.create () ~name:inner_name1 ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
  in
  let b1 = Map.find_exn inst "b" in
  let inst =
    Instantiation.create () ~name:inner_name2 ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
  in
  let b2 = Map.find_exn inst "b" in
  let circ = Circuit.create_exn ~name:"middle" [ output "b" (b1 |: b2) ] in
  let name = Circuit_database.insert ~share db circ in
  name
;;

(* [outer] *)
let outer ~cause_exn ~db ~share =
  let middle_name = middle ~cause_exn ~db ~share in
  let a = input "a" 1 in
  let inst =
    Instantiation.create () ~name:middle_name ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
  in
  let b = output "b" (Map.find_exn inst "b") in
  Circuit.create_exn ~name:"outer" [ b ]
;;

let create ~cause_exn ~share =
  let db = Circuit_database.create () in
  let outer_circ = outer ~cause_exn ~db ~share in
  print_s [%message "" ~circuit_database:(db : Circuit_database.t)];
  print_string "\nVerilog\n--------\n";
  Rtl.print ~database:db Verilog outer_circ
;;

let%expect_test "generate hierarchy without sharing" =
  create ~cause_exn:false ~share:false;
  [%expect
    {|
    (circuit_database (
      (inner  inner)
      (inner  inner_0)
      (middle middle)))

    Verilog
    --------
    module inner (
        a,
        b
    );

        input a;
        output b;

        assign b = a;

    endmodule
    module inner_0 (
        a,
        b
    );

        input a;
        output b;

        assign b = a;

    endmodule
    module middle (
        a,
        b
    );

        input a;
        output b;

        wire _6;
        wire _1;
        wire _8;
        wire _3;
        wire _9;
        inner_0
            the_inner_0
            ( .a(a),
              .b(_6) );
        assign _1 = _6;
        inner
            the_inner
            ( .a(a),
              .b(_8) );
        assign _3 = _8;
        assign _9 = _3 | _1;
        assign b = _9;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        wire _5;
        wire _2;
        middle
            the_middle
            ( .a(a),
              .b(_5) );
        assign _2 = _5;
        assign b = _2;

    endmodule
    |}]
;;

let%expect_test "generate hierarchy with sharing" =
  create ~cause_exn:false ~share:true;
  [%expect
    {|
    (circuit_database (
      (inner  inner)
      (middle middle)))

    Verilog
    --------
    module inner (
        a,
        b
    );

        input a;
        output b;

        assign b = a;

    endmodule
    module middle (
        a,
        b
    );

        input a;
        output b;

        wire _6;
        wire _1;
        wire _8;
        wire _3;
        wire _9;
        inner
            the_inner
            ( .a(a),
              .b(_6) );
        assign _1 = _6;
        inner
            the_inner_0
            ( .a(a),
              .b(_8) );
        assign _3 = _8;
        assign _9 = _3 | _1;
        assign b = _9;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        wire _5;
        wire _2;
        middle
            the_middle
            ( .a(a),
              .b(_5) );
        assign _2 = _5;
        assign b = _2;

    endmodule
    |}]
;;

let%expect_test "generate hierarchy exn" =
  show_raise (fun () -> create ~cause_exn:true ~share:true);
  [%expect
    {|
    (raised (
      "Port names are not unique"
      (circuit_name inner)
      (input_and_output_names (a))))
    |}]
;;

let%expect_test "hierarchy fold" =
  let db = Circuit_database.create () in
  let circ = outer ~db ~cause_exn:false ~share:true in
  let result =
    Hierarchy.fold circ db ~init:[] ~f:(fun all circuit inst ->
      let circuit = Option.map circuit ~f:Circuit.name in
      let inst = Option.map inst ~f:(fun i -> i.inst_name) in
      (circuit, inst) :: all)
  in
  print_s [%message (result : (string option * string option) list)];
  [%expect
    {|
    (result (
      ((inner)  (inner))
      ((inner)  (inner))
      ((middle) (middle))
      ((outer) ())))
    |}]
;;
