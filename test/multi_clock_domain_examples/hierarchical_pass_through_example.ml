(* Synchronous multiple clock domain design, where the top module instantiates an inner
   module by passing through a signal to the inner module.
*)

open Core
open Hardcaml
open Signal

module Dut = struct
  module Inner_module = struct
    module I = struct
      type 'a t =
        { clock : 'a
        ; incr : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { ctr : 'a [@bits 8] } [@@deriving hardcaml]
    end

    let create (_ : Scope.t) { I.clock; incr } =
      let spec = Reg_spec.create ~clock () in
      { O.ctr = reg_fb spec ~width:8 ~enable:incr ~f:(fun fb -> fb +:. 1) }
    ;;

    let hierarchical ?instance scope i =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical ?instance ~scope create i
    ;;
  end

  module I = struct
    type 'a t =
      { clock_fast : 'a (* x *)
      ; clock_slow : 'a (* x * 2 *)
      ; fast_incr : 'a
      ; slow_incr : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { slow_ctr : 'a [@bits 8]
      ; fast_ctr : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) { I.clock_fast; clock_slow; fast_incr; slow_incr } =
    let inner_fast =
      Inner_module.hierarchical
        ~instance:"fast"
        scope
        { clock = clock_fast; incr = fast_incr }
    in
    let inner_slow =
      Inner_module.hierarchical
        ~instance:"slow"
        scope
        { clock = clock_slow; incr = slow_incr }
    in
    { O.fast_ctr = inner_fast.ctr; slow_ctr = inner_slow.ctr }
  ;;
end

module Harness =
  Hardcaml_test_harness.Step_harness.Imperative.Make_effectful (Dut.I) (Dut.O)

module Step = Harness.Step
module Display_rule = Hardcaml_waveterm.Display_rule

let test ?(wave_width = 0) ~fast_period () =
  let slow_period = 2 * fast_period in
  Harness.run_advanced
    ~trace:`All_named
    ~print_waves_after_test:
      (Hardcaml_waveterm.For_cyclesim.Waveform.print
         ~display_width:100
         ~wave_width
         ~display_rules:
           [ Display_rule.port_name_is ~wave_format:Bit "clock_fast"
           ; Display_rule.port_name_matches ~wave_format:(Bit_or Hex) (Perl "^fast.*incr")
           ; Display_rule.port_name_matches ~wave_format:(Bit_or Hex) (Perl "^fast.*ctr")
           ; Display_rule.port_name_is ~wave_format:Bit "clock_slow"
           ; Display_rule.port_name_matches ~wave_format:(Bit_or Hex) (Perl "^slow.*incr")
           ; Display_rule.port_name_matches ~wave_format:(Bit_or Hex) (Perl "^slow.*ctr")
           ])
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
            inputs.fast_incr := Bits.vdd;
            Step.cycle h ();
            inputs.fast_incr := Bits.gnd
          done)
      in
      let task_in_slow_clock =
        Step.spawn h ~period:slow_period (fun h () ->
          for _ = 0 to 4 do
            Step.cycle h ();
            inputs.slow_incr := Bits.vdd;
            Step.cycle h ();
            inputs.slow_incr := Bits.gnd
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
      │fast_incr         ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐     │
      │                  ││────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────│
      │fast$i$incr       ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐     │
      │                  ││────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────│
      │fast_ctr          ││ 00     │01     │02     │03     │04     │05     │06     │07     │08     │09   │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────│
      │fast$o$ctr        ││ 00     │01     │02     │03     │04     │05     │06     │07     │08     │09   │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────│
      │clock_slow        ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
      │slow_incr         ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌─────│
      │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘     │
      │slow$i$incr       ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌─────│
      │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘     │
      │                  ││────────────────┬───────────────┬───────────────┬───────────────┬─────────────│
      │slow_ctr          ││ 00             │01             │02             │03             │04           │
      │                  ││────────────────┴───────────────┴───────────────┴───────────────┴─────────────│
      │                  ││────────────────┬───────────────┬───────────────┬───────────────┬─────────────│
      │slow$o$ctr        ││ 00             │01             │02             │03             │04           │
      │                  ││────────────────┴───────────────┴───────────────┴───────────────┴─────────────│
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
      │fast_incr         ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐     │
      │                  ││────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────│
      │fast$i$incr       ││    ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐     │
      │                  ││────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─────│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────│
      │fast_ctr          ││ 00     │01     │02     │03     │04     │05     │06     │07     │08     │09   │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────│
      │                  ││────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬─────│
      │fast$o$ctr        ││ 00     │01     │02     │03     │04     │05     │06     │07     │08     │09   │
      │                  ││────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴─────│
      │clock_slow        ││────┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐ │
      │                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └─│
      │slow_incr         ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌─────│
      │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘     │
      │slow$i$incr       ││        ┌───────┐       ┌───────┐       ┌───────┐       ┌───────┐       ┌─────│
      │                  ││────────┘       └───────┘       └───────┘       └───────┘       └───────┘     │
      │                  ││────────────────┬───────────────┬───────────────┬───────────────┬─────────────│
      │slow_ctr          ││ 00             │01             │02             │03             │04           │
      │                  ││────────────────┴───────────────┴───────────────┴───────────────┴─────────────│
      │                  ││────────────────┬───────────────┬───────────────┬───────────────┬─────────────│
      │slow$o$ctr        ││ 00             │01             │02             │03             │04           │
      │                  ││────────────────┴───────────────┴───────────────┴───────────────┴─────────────│
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
      │fast_incr         ││      ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     │
      │                  ││──────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────│
      │fast$i$incr       ││      ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     │
      │                  ││──────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────│
      │                  ││────────────┬───────────┬───────────┬───────────┬───────────┬───────────┬─────│
      │fast_ctr          ││ 00         │01         │02         │03         │04         │05         │06   │
      │                  ││────────────┴───────────┴───────────┴───────────┴───────────┴───────────┴─────│
      │                  ││────────────┬───────────┬───────────┬───────────┬───────────┬───────────┬─────│
      │fast$o$ctr        ││ 00         │01         │02         │03         │04         │05         │06   │
      │                  ││────────────┴───────────┴───────────┴───────────┴───────────┴───────────┴─────│
      │clock_slow        ││──────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────┐     ┌─────│
      │                  ││      └─────┘     └─────┘     └─────┘     └─────┘     └─────┘     └─────┘     │
      │slow_incr         ││            ┌───────────┐           ┌───────────┐           ┌───────────┐     │
      │                  ││────────────┘           └───────────┘           └───────────┘           └─────│
      │slow$i$incr       ││            ┌───────────┐           ┌───────────┐           ┌───────────┐     │
      │                  ││────────────┘           └───────────┘           └───────────┘           └─────│
      │                  ││────────────────────────┬───────────────────────┬───────────────────────┬─────│
      │slow_ctr          ││ 00                     │01                     │02                     │03   │
      │                  ││────────────────────────┴───────────────────────┴───────────────────────┴─────│
      │                  ││────────────────────────┬───────────────────────┬───────────────────────┬─────│
      │slow$o$ctr        ││ 00                     │01                     │02                     │03   │
      │                  ││────────────────────────┴───────────────────────┴───────────────────────┴─────│
      └──────────────────┘└──────────────────────────────────────────────────────────────────────────────┘
      |}]
  ;;
end
