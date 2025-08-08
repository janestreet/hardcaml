open Core
open Hardcaml
open Signal

let create scope ~width =
  let clock = Signal.input "clock" 1 in
  let input = input "input" width in
  let%hw test_reg = reg (Reg_spec.create ~clock ()) input in
  let output = output "output" test_reg in
  Cyclesim.create (Circuit.create_exn ~name:"Reg_test_circuit" [ output ])
;;

let gen_values ~width precent =
  let count = Percent.apply precent (Int.to_float width) |> Float.to_int in
  let shuffled_bits = List.init width ~f:Fn.id |> List.permute in
  List.take shuffled_bits count
;;

let run_test ~width precent =
  let values = gen_values ~width precent in
  Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
  let sim = Scope.create () |> create ~width in
  let input = Cyclesim.in_port sim "input" in
  Cyclesim.cycle sim;
  List.iter values ~f:(fun value ->
    Bits.( <--. ) input (1 lsl value);
    Cyclesim.cycle ~n:2 sim;
    Bits.( <--. ) input 0;
    Cyclesim.cycle ~n:2 sim);
  Cyclesim_coverage_expect_test.output_results ()
;;

let%expect_test "test one bit" =
  let width = 1 in
  run_test ~width Percent.zero;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 1
        bits never toggled ON: 0
        bits never toggled OFF: 0
    |}];
  run_test ~width (Percent.of_mult 0.5);
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 1
        bits never toggled ON: 0
        bits never toggled OFF: 0
    |}];
  run_test ~width Percent.one_hundred_percent;
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

let%expect_test "test less than one byte" =
  let width = 7 in
  run_test ~width Percent.zero;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 7
        bits never toggled ON: 0 1 2 3 4 5 6
        bits never toggled OFF: 0 1 2 3 4 5 6
    |}];
  run_test ~width (Percent.of_mult 0.5);
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 7
        bits toggled ON: 2 4 5
        bits never toggled ON: 0 1 3 6
        bits toggled OFF: 2 4 5
        bits never toggled OFF: 0 1 3 6
    |}];
  run_test ~width Percent.one_hundred_percent;
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

let%expect_test "test one byte" =
  let width = 8 in
  run_test ~width Percent.zero;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 8
        bits never toggled ON: 0 1 2 3 4 5 6 7
        bits never toggled OFF: 0 1 2 3 4 5 6 7
    |}];
  run_test ~width (Percent.of_mult 0.5);
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 8
        bits toggled ON: 1 4 6 7
        bits never toggled ON: 0 2 3 5
        bits toggled OFF: 1 4 6 7
        bits never toggled OFF: 0 2 3 5
    |}];
  run_test ~width Percent.one_hundred_percent;
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;

let%expect_test "test greater byte" =
  let width = 20 in
  run_test ~width Percent.zero;
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 20
        bits never toggled ON: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
        bits never toggled OFF: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
    |}];
  run_test ~width (Percent.of_mult 0.5);
  [%expect
    {|
    ======== Total coverage ========

    total: 0x
    circuits tested: 1
    circuits with full coverage: 0

    ====== Circuit coverage for Reg_test_circuit ======

    total coverage: 0x
    regs: 0x

    ==== Reg coverage ====

    total: 0x
    regs tested: 1
    regs with full coverage: 0

    Reg with id: 2
        names: test_reg
        width: 20
        bits toggled ON: 0 2 4 6 7 10 12 14 16 17
        bits never toggled ON: 1 3 5 8 9 11 13 15 18 19
        bits toggled OFF: 0 2 4 6 7 10 12 14 16 17
        bits never toggled OFF: 1 3 5 8 9 11 13 15 18 19
    |}];
  run_test ~width Percent.one_hundred_percent;
  [%expect
    {|
    ======== Total coverage ========

    total: 1x
    circuits tested: 1
    circuits with full coverage: 1
    |}]
;;
