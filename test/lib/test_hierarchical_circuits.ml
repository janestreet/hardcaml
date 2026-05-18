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
  let b1 = Instantiation.output inst "b" in
  let inst =
    Instantiation.create () ~name:inner_name2 ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
  in
  let b2 = Instantiation.output inst "b" in
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
  let b = output "b" (Instantiation.output inst "b") in
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
      (inner  inner_1)
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
    module inner_1 (
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

        wire signal_inst;
        wire signal_wire;
        wire signal_inst_1;
        wire signal_wire_1;
        wire signal_or;
        inner_1
            the_inner_1
            ( .a(a),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        inner
            the_inner
            ( .a(a),
              .b(signal_inst_1) );
        assign signal_wire_1 = signal_inst_1;
        assign signal_or = signal_wire_1 | signal_wire;
        assign b = signal_or;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        wire signal_inst;
        wire signal_wire;
        middle
            the_middle
            ( .a(a),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        assign b = signal_wire;

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

        wire signal_inst;
        wire signal_wire;
        wire signal_inst_1;
        wire signal_wire_1;
        wire signal_or;
        inner
            the_inner
            ( .a(a),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        inner
            the_inner_1
            ( .a(a),
              .b(signal_inst_1) );
        assign signal_wire_1 = signal_inst_1;
        assign signal_or = signal_wire_1 | signal_wire;
        assign b = signal_or;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        wire signal_inst;
        wire signal_wire;
        middle
            the_middle
            ( .a(a),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        assign b = signal_wire;

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
      let inst = Option.map inst ~f:(fun i -> i.circuit_name) in
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
