open! Import
open Signal

let rtl_write_null outputs = Rtl.print Verilog (Circuit.create_exn ~name:"test" outputs)

let%expect_test "Port names must be unique" =
  require_does_raise (fun () -> rtl_write_null [ output "a" (input "a" 1) ]);
  [%expect
    {| ("Port names are not unique" (circuit_name test) (input_and_output_names (a))) |}]
;;

let%expect_test "Port names must be legal" =
  require_does_raise (fun () -> rtl_write_null [ output "a" (input "1^7" 1) ]);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name test)
      (hierarchy_path (test))
      (exn (
        "[Illegal port name"
        (name       1^7)
        (legal_name _1_7)
        (note       "Hardcaml will not change ports names.")
        (port ((wire (names (1^7)) (width 1)))))))
    |}]
;;

let%expect_test "Port name clashes with reserved name" =
  require_does_raise (fun () -> rtl_write_null [ output "generate" (input "x" 1) ]);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name test)
      (hierarchy_path (test))
      (exn (
        "Port name has already been defined"
        (name generate)
        (port ((
          wire
          (names (generate))
          (width   1)
          (data_in x)))))))
    |}]
;;

let%expect_test "output wire is width 0 (or empty)" =
  (* The exception is raised by the [output] function. *)
  require_does_raise (fun () -> rtl_write_null [ output "x" empty ]);
  [%expect {| ("Width of signals must be >= 0" (width 0)) |}]
;;

let%expect_test "instantiation input is empty" =
  require_does_raise (fun () ->
    let a = Signal.empty in
    let inst =
      Instantiation.create () ~name:"example" ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
    in
    Circuit.create_exn
      ~name:"example"
      [ Signal.output "b" (Instantiation.output inst "b") ]
    |> Rtl.print Verilog);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name example)
      (hierarchy_path (example))
      (exn (
        "[Rtl_ast] failed to create statement for signal"
        (signal (
          instantiation
          (width 1)
          ("work.example(rtl){the_example}"
            (parameters ())
            (inputs  ((a empty)))
            (outputs ((b 1))))))
        (exn (
          "[Rtl_ast] Failed to find signal in logic map"
          (context "Inst.input_port: a"))))))
    |}]
;;

let test_multiple_circuits top_names =
  let module A = struct
    type 'a t = { a : 'a } [@@deriving hardcaml]
  end
  in
  let module B = struct
    type 'a t = { b : 'a } [@@deriving hardcaml]
  end
  in
  let module Inner = struct
    module I = A
    module O = B

    let create _scope (i : _ I.t) = { O.b = ~:(i.a) }

    let hierarchy scope i =
      let module Hier = Hierarchy.In_scope (I) (O) in
      Hier.hierarchical ~name:"inner" ~scope create i
    ;;
  end
  in
  let module Top = struct
    module I = A
    module O = B

    let create scope i = Inner.hierarchy scope i
  end
  in
  let module Circuit = Circuit.With_interface (Top.I) (Top.O) in
  let scope = Scope.create () in
  let circuits =
    List.map top_names ~f:(fun name -> Circuit.create_exn ~name (Top.create scope))
  in
  Rtl.create ~database:(Scope.circuit_database scope) Verilog circuits
  |> Rtl.full_hierarchy
  |> Rope.to_string
  |> Stdio.print_string
;;

let%expect_test "multiple circuits - inner component is shared" =
  test_multiple_circuits [ "top1"; "top2" ];
  [%expect
    {|
    module inner (
        a,
        b
    );

        input a;
        output b;

        wire _2;
        wire _4;
        assign _2 = a;
        assign _4 = ~ _2;
        assign b = _4;

    endmodule
    module top1 (
        a,
        b
    );

        input a;
        output b;

        wire _2;
        wire _5;
        wire _3;
        assign _2 = a;
        inner
            inner
            ( .a(_2),
              .b(_5) );
        assign _3 = _5;
        assign b = _3;

    endmodule
    module top2 (
        a,
        b
    );

        input a;
        output b;

        wire _2;
        wire _5;
        wire _3;
        assign _2 = a;
        inner
            inner_1
            ( .a(_2),
              .b(_5) );
        assign _3 = _5;
        assign b = _3;

    endmodule
    |}]
;;

let%expect_test "same name in multiple top level circuits" =
  require_does_raise (fun () -> test_multiple_circuits [ "top"; "top" ]);
  [%expect {| ("Top level circuit name has already been used" (name top)) |}]
;;
