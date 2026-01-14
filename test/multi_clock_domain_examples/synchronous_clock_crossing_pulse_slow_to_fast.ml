(** Test demonstrating a synchronous clock domain crossing from fast -> slow clock domain *)

open Core
open Hardcaml
open Signal

module Dut = struct
  module I = struct
    type 'a t =
      { clock_fast : 'a (* x *)
      ; clock_slow : 'a (* x * 2 *)
      ; pulse_slow : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { pulse_fast : 'a [@bits 1]
      ; counter_slow : 'a [@bits 8]
      (** A free running counter that counts in the slow clock *)
      }
    [@@deriving hardcaml]
  end

  let create (_ : Scope.t) { I.clock_fast; clock_slow; pulse_slow } =
    let spec_fast = Reg_spec.create ~clock:clock_fast () in
    let spec_slow = Reg_spec.create ~clock:clock_slow () in
    let pulse_fast = pulse_slow |> pipeline ~n:0 spec_fast |> reg spec_fast in
    let counter_slow = reg_fb spec_slow ~width:8 ~f:(fun fb -> fb +:. 1) in
    { O.counter_slow; pulse_fast }
  ;;
end

module Harness =
  Hardcaml_test_harness.Step_harness.Imperative.Make_effectful (Dut.I) (Dut.O)

module Step = Harness.Step

let test ?(wave_width = 0) ~fast_period () =
  let slow_period = 2 * fast_period in
  Harness.run_advanced
    ~print_waves_after_test:
      (Hardcaml_waveterm.For_cyclesim.Waveform.print ~display_width:80 ~wave_width)
    ~clock_mode:
      (By_input_clocks
         [ { name = Cyclesim_clock_domain.Name.of_string "clock_fast"
           ; period = fast_period
           }
         ; { name = Cyclesim_clock_domain.Name.of_string "clock_slow"
           ; period = slow_period
           }
         ])
    ~create:Dut.create
    (fun h sim ->
      let inputs = Cyclesim.inputs sim in
      let task_in_slow_clock =
        Step.spawn h ~period:slow_period (fun h () ->
          Step.cycle ~num_cycles:1 h ();
          Step.cycle ~num_cycles:1 h ();
          inputs.pulse_slow := Bits.vdd;
          Step.cycle ~num_cycles:1 h ();
          inputs.pulse_slow := Bits.gnd;
          Step.cycle ~num_cycles:3 h ())
      in
      Step.wait_for h task_in_slow_clock)
;;

module%test [@tags "runtime5-only"] _ = struct
  let%expect_test "fast period 1" =
    test ~fast_period:1 ();
    [%expect
      {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
      │clock_fast        ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐   │
      │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─  │
      │clock_slow        ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐     │
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───  │
      │pulse_slow        ││                ┌───────┐                                 │
      │                  ││────────────────┘       └───────────────────────────────  │
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────  │
      │counter_slow      ││ 00     │01     │02     │03     │04     │05     │06       │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────  │
      │pulse_fast        ││                    ┌───────┐                             │
      │                  ││────────────────────┘       └───────────────────────────  │
      └──────────────────┘└──────────────────────────────────────────────────────────┘
      |}]
  ;;

  let%expect_test "fast period 2" =
    test ~fast_period:2 ();
    [%expect
      {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
      │clock_fast        ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐       │
      │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─      │
      │clock_slow        ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───      │
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘         │
      │pulse_slow        ││                ┌───────┐                                 │
      │                  ││────────────────┘       └───────────────────────────      │
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───      │
      │counter_slow      ││ 00     │01     │02     │03     │04     │05     │06       │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───      │
      │pulse_fast        ││                    ┌───────┐                             │
      │                  ││────────────────────┘       └───────────────────────      │
      └──────────────────┘└──────────────────────────────────────────────────────────┘
      |}]
  ;;

  let%expect_test "fast period 3" =
    test ~fast_period:3 ~wave_width:(-1) ();
    [%expect
      {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
      │clock_fast        ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐│
      │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └│
      │clock_slow        ││──────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐   │
      │                  ││      └─────┘     └─────┘     └─────┘     └─────┘     └───│
      │pulse_slow        ││                        ┌───────────┐                     │
      │                  ││────────────────────────┘           └─────────────────────│
      │                  ││────────────┬───────────┬───────────┬───────────┬─────────│
      │counter_slow      ││ 00         │01         │02         │03         │04       │
      │                  ││────────────┴───────────┴───────────┴───────────┴─────────│
      │pulse_fast        ││                              ┌───────────┐               │
      │                  ││──────────────────────────────┘           └───────────────│
      └──────────────────┘└──────────────────────────────────────────────────────────┘
      |}]
  ;;
end
