open! Core
open! Hardcaml
open Signal

let%expect_test "Initial values, resets and clears of registers" =
  let clock = input "clock" 1 in
  let reset = input "reset" 1 in
  let clear = input "clear" 1 in
  let q =
    reg_fb
      (Reg_spec.create ~clock ~reset ~clear ())
      ~initialize_to:(Signal.of_int ~width:8 16)
      ~reset_to:(Signal.of_int ~width:8 32)
      ~clear_to:(Signal.of_int ~width:8 48)
      ~width:8
      ~f:(fun d -> d +:. 1)
  in
  let circ = Circuit.create_exn ~name:"initial" [ output "q" q ] in
  let sim = Cyclesim.create circ in
  let waves, sim = Hardcaml_waveterm_cyclesim.Waveform.create sim in
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Cyclesim.reset sim;
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Cyclesim.in_port sim "clear" := Bits.vdd;
  Cyclesim.cycle sim;
  Cyclesim.in_port sim "clear" := Bits.gnd;
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Hardcaml_waveterm_cyclesim.Waveform.print waves ~wave_width:2 ~display_height:12;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │reset          ││            ┌─────┐                                │
    │               ││────────────┘     └─────────────────────────────   │
    │clear          ││                              ┌─────┐              │
    │               ││──────────────────────────────┘     └───────────   │
    │               ││──────┬───────────┬─────┬─────┬─────┬─────┬─────   │
    │q              ││ 10   │11         │20   │21   │22   │30   │31      │
    │               ││──────┴───────────┴─────┴─────┴─────┴─────┴─────   │
    │               ││                                                   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
