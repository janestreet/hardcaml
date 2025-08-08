open Core
open Hardcaml
open Signal
open Always

let test ?(debug = false) ~build_output ~run () =
  Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
  let scope = Scope.create () in
  let clock = Signal.input "clock" 1 in
  let clear = Signal.input "clear" 1 in
  let output_signal = build_output scope ~clock ~clear in
  let output = output "output" output_signal in
  let circuit = Circuit.create_exn ~name:"Test_circuit" [ output ] in
  if debug
  then
    Circuit.signal_graph circuit
    |> Signal_graph.iter ~f:(fun s -> print_s [%message (s : Signal.t)]);
  let sim = Cyclesim.create circuit in
  let clear = Cyclesim.in_port sim "clear" in
  clear := Bits.vdd;
  Cyclesim.cycle sim;
  clear := Bits.gnd;
  run sim;
  Cyclesim_coverage_expect_test.output_results ()
;;

let%expect_test "switch" =
  let module State = struct
    type t =
      | A
      | B
      | C
    [@@deriving compare ~localize, enumerate, sexp_of, variants]
  end
  in
  let create_sm reg_spec encoding =
    State_machine.create (module State) reg_spec ~encoding ~enable:vdd
  in
  let test encoding =
    test
      ~build_output:(fun scope ~clock ~clear ->
        let reg_spec = Reg_spec.create ~clock ~clear () in
        let out_var = Variable.wire ~default:gnd () in
        let%hw out = out_var.value in
        let sm = create_sm reg_spec encoding in
        compile
          [ sm.switch
              [ A, [ out_var <--. 1; sm.set_next B ]
              ; B, [ out_var <--. 2; sm.set_next C ]
              ; C, [ out_var <--. 3; sm.set_next A ]
              ]
          ];
        out)
      ~run:(fun sim -> Cyclesim.cycle sim ~n:100)
      ()
  in
  List.iter [ State_machine.Encoding.Binary; Gray ] ~f:(fun encoding ->
    test encoding;
    [%expect
      {|
      ======== Total coverage ========

      total: 1x
      circuits tested: 1
      circuits with full coverage: 1
      |}]);
  test Onehot;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Test_circuit ======

    total coverage: 85.7143%
    muxes: 1x
    always_states: 0x

    ==== Mux coverage ====

    total: 1x
    muxes tested: 6
    muxes with full coverage: 6

    ==== Always state coverage ====

    total: 0x
    always states tested: 1
    always states with full coverage: 0

    Always state with id: 5
        saw transitions:
            A -> B
            B -> C
            C -> A
        never saw:
            A -> A
            B -> B
            C -> C
    |}]
;;

let%expect_test "switch - self transition" =
  let module State = struct
    type t =
      | A
      | B
    [@@deriving compare ~localize, enumerate, sexp_of, variants]
  end
  in
  let create_sm reg_spec encoding =
    State_machine.create (module State) reg_spec ~encoding ~enable:vdd
  in
  let test encoding =
    test
      ~build_output:(fun scope ~clock ~clear ->
        let reg_spec = Reg_spec.create ~clock ~clear () in
        let out_var = Variable.wire ~default:gnd () in
        let%hw out = out_var.value in
        let sm = create_sm reg_spec encoding in
        compile
          [ sm.switch
              [ A, [ out_var <--. 1; sm.set_next B ]
              ; B, [ out_var <--. 3; sm.set_next B ]
              ]
          ];
        out)
      ~run:(fun sim -> Cyclesim.cycle sim ~n:100)
      ()
  in
  List.iter [ State_machine.Encoding.Binary; Gray ] ~f:(fun encoding ->
    test encoding;
    [%expect
      {|
      ======== Total coverage ========

      total: 1x
      circuits tested: 1
      circuits with full coverage: 1
      |}]);
  test Onehot;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Test_circuit ======

    total coverage: 80%
    muxes: 1x
    always_states: 0x

    ==== Mux coverage ====

    total: 1x
    muxes tested: 4
    muxes with full coverage: 4

    ==== Always state coverage ====

    total: 0x
    always states tested: 1
    always states with full coverage: 0

    Always state with id: 5
        saw transitions:
            A -> B
            B -> B
        never saw:
            A -> A
    |}]
;;
