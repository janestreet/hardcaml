open Core
open Hardcaml
open Hardcaml_waveterm_kernel
open Hardcaml_waveterm_cyclesim

module Async_fifo = Async_fifo.Make (struct
    let width = 16
    let log2_depth = 2
    let optimize_for_same_clock_rate_and_always_reading = true
  end)

module Sim = Cyclesim.With_interface (Async_fifo.I) (Async_fifo.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true () in
  Sim.create (Async_fifo.create ~scope)
;;

let test ~probability_write_enable () =
  let waves, sim = Waveform.create (create_sim ()) in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  inputs.read_enable := Bits.gnd;
  let q_write = Queue.create () in
  let q_read = Queue.create () in
  let cycle () =
    Cyclesim.cycle sim;
    if Bits.to_bool !(outputs.valid)
    then Queue.enqueue q_read (Bits.to_int_trunc !(outputs.data_out))
  in
  let rec deassert_write_enable_loop () =
    if Float.(Random.float 1.0 >. probability_write_enable)
    then (
      inputs.write_enable := Bits.gnd;
      cycle ();
      deassert_write_enable_loop ())
  in
  for i = 1 to 8 do
    deassert_write_enable_loop ();
    inputs.data_in := Bits.of_int_trunc i ~width:16;
    inputs.write_enable := Bits.vdd;
    Queue.enqueue q_write i;
    cycle ()
  done;
  inputs.write_enable := Bits.gnd;
  for _ = 1 to 6 do
    cycle ()
  done;
  let display_rules =
    Display_rule.
      [ port_name_is ~alignment:Right "data_in" ~wave_format:Hex
      ; port_name_is ~alignment:Right "write_enable" ~wave_format:Bit
      ; port_name_is ~alignment:Right "almost_empty" ~wave_format:Bit
      ; port_name_is ~alignment:Right "data_out" ~wave_format:Hex
      ; port_name_is ~alignment:Right "valid" ~wave_format:Bit
      ]
  in
  Waveform.print waves ~display_rules ~wave_width:1 ~display_width:90;
  [%test_result: int Queue.t] ~expect:q_write q_read
;;

let%expect_test "Synchronous case works with line-rate data" =
  test ~probability_write_enable:1.0 ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───┬───┬───┬───┬───┬───────────────────────────            │
    │data_in           ││ .01│.02│.03│.04│.05│.06│.07│0008                                   │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───────────────────────────            │
    │write_enable      ││────────────────────────────────┐                                   │
    │                  ││                                └───────────────────────            │
    │almost_empty      ││────────────────────────────────────────────────────────            │
    │                  ││                                                                    │
    │                  ││────────┬───────┬───┬───┬───┬───┬───┬───┬───┬───────────            │
    │data_out          ││ 0000   │0001   │.02│.03│.04│.05│.06│.07│.08│0005                   │
    │                  ││────────┴───────┴───┴───┴───┴───┴───┴───┴───┴───────────            │
    │valid             ││            ┌───────────────────────────────┐                       │
    │                  ││────────────┘                               └───────────            │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Synchronous case works with non line-rate data" =
  test ~probability_write_enable:0.6 ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
    │                  ││────────────┬───┬───┬───────┬───┬───┬───────────────┬───────────┬───│
    │data_in           ││ 0000       │.01│.02│0003   │.04│.05│0006           │0007       │.08│
    │                  ││────────────┴───┴───┴───────┴───┴───┴───────────────┴───────────┴───│
    │write_enable      ││            ┌───────────┐   ┌───────────┐           ┌───┐       ┌───│
    │                  ││────────────┘           └───┘           └───────────┘   └───────┘   │
    │almost_empty      ││────────────────────────────────────────────────────────────────────│
    │                  ││                                                                    │
    │                  ││────────────────────┬───────┬───┬───┬───────┬───┬───┬───────┬───────│
    │data_out          ││ 0000               │0001   │.02│.03│0004   │.05│.06│0003   │0007   │
    │                  ││────────────────────┴───────┴───┴───┴───────┴───┴───┴───────┴───────│
    │valid             ││                        ┌───────────┐   ┌───────────┐           ┌───│
    │                  ││────────────────────────┘           └───┘           └───────────┘   │
    └──────────────────┘└────────────────────────────────────────────────────────────────────┘
    |}]
;;
