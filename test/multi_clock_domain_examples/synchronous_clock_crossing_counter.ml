(* Some examples to demonstrate synchronous clock crossing *)

open Core
open Hardcaml
open Signal

module Dut = struct
  module I = struct
    type 'a t =
      { clock_fast : 'a (* x *)
      ; clock_slow : 'a (* x * 2 *)
      ; incr_fast : 'a
      ; incr_slow : 'a
      }
    [@@deriving hardcaml]
  end

  (* Both counters increase whenever one of [incr_fast] or [incr_slow] is asserted at the
     rising edge. *)
  module O = struct
    type 'a t =
      { ctr_slow : 'a [@bits 8]
      ; ctr_fast : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  let create (_ : Scope.t) { I.clock_fast; clock_slow; incr_fast; incr_slow } =
    let spec_fast = Reg_spec.create ~clock:clock_fast () in
    let spec_slow = Reg_spec.create ~clock:clock_slow () in
    let ctr_fast =
      reg_fb spec_fast ~width:8 ~enable:(incr_fast |: incr_slow) ~f:(fun fb -> fb +:. 1)
    in
    let ctr_slow =
      reg_fb spec_slow ~width:8 ~enable:(incr_fast |: incr_slow) ~f:(fun fb -> fb +:. 1)
    in
    { O.ctr_fast; ctr_slow }
  ;;
end

module Harness =
  Hardcaml_test_harness.Step_harness.Imperative.Make_effectful (Dut.I) (Dut.O)

module Step = Harness.Step

let test ?(wave_width = 0) ~fast_period () =
  let slow_period = 2 * fast_period in
  Harness.run_advanced
    ~print_waves_after_test:
      (Hardcaml_waveterm.For_cyclesim.Waveform.print ~display_width:100 ~wave_width)
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
      let task_in_fast_clock =
        Step.spawn h ~period:fast_period (fun h () ->
          for _ = 0 to 8 do
            Step.cycle h ();
            inputs.incr_fast := Bits.vdd;
            Step.cycle h ();
            inputs.incr_fast := Bits.gnd
          done)
      in
      let task_in_slow_clock =
        Step.spawn h ~period:slow_period (fun h () ->
          for _ = 0 to 4 do
            Step.cycle h ();
            inputs.incr_slow := Bits.vdd;
            Step.cycle h ();
            inputs.incr_slow := Bits.gnd
          done)
      in
      Step.wait_for h task_in_fast_clock;
      Step.wait_for h task_in_slow_clock)
;;

module%test [@tags "runtime5-only"] _ = struct
  let%expect_test "period 1 and 2" =
    test ~fast_period:1 ();
    [%expect
      {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
      │clock_fast        ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
      │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
      │clock_slow        ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
      │incr_fast         ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐     │
      │                  ││────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────│
      │incr_slow         ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌─────│
      │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘     │
      │                  ││────────┬───┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬─│
      │ctr_fast          ││ 00     │01 │02 │03     │04 │05 │06     │07 │08 │09     │0A │0B │0C     │0D │.│
      │                  ││────────┴───┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴─│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────│
      │ctr_slow          ││ 00     │01     │02     │03     │04     │05     │06     │07     │08     │09   │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────│
      └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;

  let%expect_test "period 2 and 4" =
    test ~fast_period:2 ();
    [%expect
      {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
      │clock_fast        ││──┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
      │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
      │clock_slow        ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
      │incr_fast         ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐     │
      │                  ││────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────│
      │incr_slow         ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌─────│
      │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘     │
      │                  ││────────┬───┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬───┬───────┬───┬─│
      │ctr_fast          ││ 00     │01 │02 │03     │04 │05 │06     │07 │08 │09     │0A │0B │0C     │0D │.│
      │                  ││────────┴───┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴───┴───────┴───┴─│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────│
      │ctr_slow          ││ 00     │01     │02     │03     │04     │05     │06     │07     │08     │09   │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────│
      └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;

  let%expect_test "period 3 and 6" =
    test ~fast_period:3 ~wave_width:(-1) ();
    [%expect
      {|
      ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────┐
      │clock_fast        ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  │
      │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──│
      │clock_slow        ││──────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────│
      │                  ││      └─────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────┘     │
      │incr_fast         ││      ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     │
      │                  ││──────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────│
      │incr_slow         ││            ┌───────────┐           ┌───────────┐           ┌───────────┐     │
      │                  ││────────────┘           └───────────┘           └───────────┘           └─────│
      │                  ││────────────┬─────┬─────┬───────────┬─────┬─────┬───────────┬─────┬─────┬─────│
      │ctr_fast          ││ 00         │01   │02   │03         │04   │05   │06         │07   │08   │09   │
      │                  ││────────────┴─────┴─────┴───────────┴─────┴─────┴───────────┴─────┴─────┴─────│
      │                  ││────────────┬───────────┬───────────┬───────────┬───────────┬───────────┬─────│
      │ctr_slow          ││ 00         │01         │02         │03         │04         │05         │06   │
      │                  ││────────────┴───────────┴───────────┴───────────┴───────────┴───────────┴─────│
      └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;
end
