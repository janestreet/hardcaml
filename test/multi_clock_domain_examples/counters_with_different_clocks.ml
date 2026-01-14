open! Core
open Hardcaml
open Signal
open Hardcaml_event_driven_sim.Two_state_simulator

module I = struct
  type 'a t =
    { clock : 'a
    ; enable : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { reg_out : 'a [@bits 8] } [@@deriving hardcaml]
end

let rising_edge ({ clock; enable } : _ I.t) =
  { O.reg_out = reg_fb (Reg_spec.create ~clock ()) ~enable ~width:8 ~f:(fun d -> d +:. 1)
  }
;;

let falling_edge ({ clock; enable } : _ I.t) =
  { O.reg_out =
      reg_fb
        (Reg_spec.create ~clock () |> Reg_spec.override ~clock_edge:Falling)
        ~enable
        ~width:8
        ~f:(fun d -> d +:. 1)
  }
;;

let gated_clock ({ clock; enable } : _ I.t) =
  let gated_clock = (clock &: enable) -- "gated clock" in
  { O.reg_out =
      reg_fb (Reg_spec.create ~clock:gated_clock ()) ~width:8 ~f:(fun d -> d +:. 1)
  }
;;

let falling_edge_enable_reg ({ clock; enable } : _ I.t) =
  let spec = Reg_spec.create ~clock () |> Reg_spec.override ~clock_edge:Falling in
  let enable_reg = reg spec enable -- "enable reg" in
  { O.reg_out = reg_fb spec ~enable:enable_reg ~width:8 ~f:(fun d -> d +:. 1) }
;;

let inverted_clock ({ clock; enable } : _ I.t) =
  let inverted_clock = ~:clock -- "~clock" in
  { O.reg_out =
      reg_fb (Reg_spec.create ~clock:inverted_clock ()) ~enable ~width:8 ~f:(fun d ->
        d +:. 1)
  }
;;

let run_cyclesim_test circuit =
  let module Cyclesim_waveform = Hardcaml_waveterm.For_cyclesim.Waveform in
  let waves, sim =
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create circuit in
    Cyclesim_waveform.create sim
  in
  let inputs = Cyclesim.inputs sim in
  for i = 1 to 12 do
    if i mod 2 = 0
    then inputs.enable := Bits.of_bool false
    else inputs.enable := Bits.of_bool true;
    Cyclesim.cycle sim
  done;
  Cyclesim_waveform.print waves ~wave_width:3 ~display_width:82
;;

let run_event_sim_test circuit =
  let module Sim_interface = With_interface (I) (O) in
  let waves, { Sim_interface.simulator = sim; _ } =
    Sim_interface.with_waveterm ~config:Config.trace_all circuit (fun input _output ->
      let open Logic in
      let open Simulator in
      let input = I.map input ~f:(fun v -> v.signal) in
      [ Sim_interface.create_clock input.I.clock ~time:1 ~initial_delay:0
      ; Async.create_process (fun () ->
          Async.forever (fun () ->
            input.I.enable <-- of_bool true;
            let%bind.Async.Deferred () = Async.delay 2 in
            input.I.enable <-- of_bool false;
            Async.delay 2))
      ])
  in
  Simulator.run ~time_limit:20 sim;
  Hardcaml_event_driven_sim.Waveterm.Waveform.expect waves ~wave_width:1 ~display_width:82
;;

let run_test circuit =
  print_endline "Cyclesim result";
  run_cyclesim_test circuit;
  print_endline "Event sim result";
  run_event_sim_test circuit
;;

let%expect_test "rising edge" =
  (* This test is different because it uses the last value of enable on the clock rising
     edge *)
  run_test rising_edge;
  [%expect
    {|
    Cyclesim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───│
    │reg_out           ││ 00     │01             │02             │03             │04 │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    Event sim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────────────────┬───────────────┬───────────────┬───────────│
    │reg_out           ││ 00             │01             │02             │03         │
    │                  ││────────────────┴───────────────┴───────────────┴───────────│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    af6143bb92c8204c7d94352b47425ff4
    |}]
;;

(* Cyclesim gets these following tests wrong becuase it doesn't know about clocks *)

let%expect_test "falling edge" =
  (* Cyclesim still writes to [reg_out] on the Rising edge because it doesn't know about
     Falling edges; it only writes to a register on the rising edge *)
  run_test falling_edge;
  [%expect
    {|
    Cyclesim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───│
    │reg_out           ││ 00     │01             │02             │03             │04 │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    Event sim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────┬───────────────┬───────────────┬───────────────┬───────│
    │reg_out           ││ 00 │01             │02             │03             │04     │
    │                  ││────┴───────────────┴───────────────┴───────────────┴───────│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    69b589fef15636ef82429349cc133622
    |}]
;;

let%expect_test "inverted clock" =
  (* Cyclesim still writes to [reg_out] on the Rising edge because it doesn't know that
     the register is clocked with ~clock. *)
  run_test inverted_clock;
  [%expect
    {|
    Cyclesim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────────┬───────────────┬───────────────┬───────────────┬───│
    │reg_out           ││ 00     │01             │02             │03             │04 │
    │                  ││────────┴───────────────┴───────────────┴───────────────┴───│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    Event sim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────┬───────────────┬───────────────┬───────────────┬───────│
    │reg_out           ││ 00 │01             │02             │03             │04     │
    │                  ││────┴───────────────┴───────────────┴───────────────┴───────│
    │~clock            ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
    │                  ││────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    7e61f0a9614f327cfe5a5c1574340153
    |}]
;;

let%expect_test "gated clock" =
  (* Cyclesim still writes to [reg_out] on every Rising edge *)
  run_test gated_clock;
  [%expect
    {|
    Cyclesim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───│
    │reg_out           ││ 00     │01     │02     │03     │04     │05     │06     │07 │
    │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    Event sim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │gated clock       ││────┐           ┌───┐           ┌───┐           ┌───┐       │
    │                  ││    └───────────┘   └───────────┘   └───────────┘   └───────│
    │                  ││────────────────┬───────────────┬───────────────┬───────────│
    │reg_out           ││ 00             │01             │02             │03         │
    │                  ││────────────────┴───────────────┴───────────────┴───────────│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    ac097252976342f7f663fe13e8b04bfd
    |}]
;;

let%expect_test "falling edge - enable as a reg" =
  (* Same as the previous falling edge test - Cyclesim still writes to the regs on the
     [Rising] edge *)
  run_test falling_edge_enable_reg;
  [%expect
    {|
    Cyclesim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │                  ││────────────────┬───────────────┬───────────────┬───────────│
    │reg_out           ││ 00             │01             │02             │03         │
    │                  ││────────────────┴───────────────┴───────────────┴───────────│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    Event sim result
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
    │clock             ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───│
    │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   │
    │enable            ││────────┐       ┌───────┐       ┌───────┐       ┌───────┐   │
    │                  ││        └───────┘       └───────┘       └───────┘       └───│
    │enable reg        ││    ┌───────┐       ┌───────┐       ┌───────┐       ┌───────│
    │                  ││────┘       └───────┘       └───────┘       └───────┘       │
    │                  ││────────────┬───────────────┬───────────────┬───────────────│
    │reg_out           ││ 00         │01             │02             │03             │
    │                  ││────────────┴───────────────┴───────────────┴───────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────┘
    27cbf5428b03c61e7156ea640c786f99
    |}]
;;
