# Waveforms

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

In the [simple counter](counter_example.md) example we printed the
value of the output ports during the simulation. Printing outputs or
even modelling the expected behaviour of a circuit dynamically is
sometimes the best way to test it. However, the majority of the time
we just want to visualize the result. For that, we can use waveforms.

The [`hardcaml_waveterm`](https://github.com/janestreet/hardcaml_waveterm)
library can capture and print waveforms from
Hardcaml simulations.

## Printing results as waveforms

We can capture a waveform with
[`Waveform.create`](https://ocaml.org/p/hardcaml_waveterm/latest/doc/Hardcaml_waveterm/index.html).
It takes as an argument a simulator and returns a waveform and
modified simulator that captures the input and output port values.

The waveform can be displayed with `Waveform.print`.

<!--
Include the counter design.

```ocaml
open Hardcaml
open Hardcaml.Signal
open Hardcaml_waveterm

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; incr : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { dout : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end

let create (i : _ I.t) =
    { O.dout =
        reg_fb
          (Reg_spec.create ~clock:i.clock ~clear:i.clear ())
          ~enable:i.incr
          ~width:8
          ~f:(fun d -> d +:. 1)
    }
;;

module Simulator = Cyclesim.With_interface(I)(O)

let testbench sim =
  let inputs : _ I.t = Cyclesim.inputs sim in
  let step ~clear ~incr =
    inputs.clear := if clear=1 then Bits.vdd else Bits.gnd;
    inputs.incr := if incr=1 then Bits.vdd else Bits.gnd;
    Cyclesim.cycle sim
  in
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:1;
  step ~clear:0 ~incr:1;
  step ~clear:1 ~incr:0;
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:0
;;
```
-->

```ocaml
# let testbench () =
    let sim = Simulator.create create in
    let waves, sim = Waveform.create sim in
    testbench sim;
    waves
val testbench : unit -> Waveform.t = <fun>

# let waves = testbench ()
val waves : Waveform.t = <abstr>
# Waveform.print ~display_height:12 waves
┌Signals────────┐┌Waves──────────────────────────────────────────────┐
│clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
│               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
│clear          ││                        ┌───────┐                  │
│               ││────────────────────────┘       └───────────────   │
│incr           ││        ┌───────────────┐                          │
│               ││────────┘               └───────────────────────   │
│               ││────────────────┬───────┬───────┬───────────────   │
│dout           ││ 00             │01     │02     │00                │
│               ││────────────────┴───────┴───────┴───────────────   │
│               ││                                                   │
└───────────────┘└───────────────────────────────────────────────────┘
- : unit = ()
```

## Capturing in expect tests

Waveforms can be captured as expect test output.

```ocaml skip
let%expect_test "counter" =
  let waves = testbench ()
  Waveform.print ~display_height:12 waves
  [%expect {|
┌Signals────────┐┌Waves──────────────────────────────────────────────┐
│clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
│               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
│clear          ││                        ┌───────┐                  │
│               ││────────────────────────┘       └───────────────   │
│incr           ││        ┌───────────────┐                          │
│               ││────────┘               └───────────────────────   │
│               ││────────────────┬───────┬───────┬───────────────   │
│dout           ││ 00             │01     │02     │00                │
│               ││────────────────┴───────┴───────┴───────────────   │
│               ││                                                   │
└───────────────┘└───────────────────────────────────────────────────┘
  |}]
```

## Configuration options

The `Waveform.print` function takes optional arguments which control the rendering of the waveform.

- `start_cycle` first cycle to display
- `display_width`, `display_height` width and height of the waveform
- `wave_width` scale at which the waveform is shown (negative values allowed)
- `display_rules` configuration of the signals to show

## Display rules

```ocaml
# Waveform.print
    ~display_height:10
    ~display_rules:
      Display_rule.[ port_name_is "dout" ~wave_format:Unsigned_int
                   ; port_name_matches Re.Posix.(compile (re "cl.*")) ~wave_format:Bit ]
    waves
┌Signals────────┐┌Waves──────────────────────────────────────────────┐
│               ││────────────────┬───────┬───────┬───────────────   │
│dout           ││ 0              │1      │2      │0                 │
│               ││────────────────┴───────┴───────┴───────────────   │
│clear          ││                        ┌───────┐                  │
│               ││────────────────────────┘       └───────────────   │
│clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
│               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
│               ││                                                   │
└───────────────┘└───────────────────────────────────────────────────┘
- : unit = ()
```

The signals are shown in the order of the first matching
[`display_rule`](https://ocaml.org/p/hardcaml_waveterm/latest/doc/Hardcaml_waveterm/Display_rule/index.html).
The way the value is shown is configured with the
[`wave_format`](https://ocaml.org/p/hardcaml_waveterm/latest/doc/Hardcaml_waveterm/Wave_format/index.html)
argument. Groups of signals can be specified using regular expressions.
