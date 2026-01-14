# 3.4 Coverage

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
# open Hardcaml
# open Coverage_prim
```
-->
<!--
```ocaml
# let clock = Signal.input "clock" 1;;
val clock : Signal.t = (wire (names (clock)) (width 1))
# let const i = Signal.of_int_trunc i ~width:2 ;;
val const : int -> Signal.t = <fun>
# let run s =
    let output = Signal.output "output" s in
    let circuit = Circuit.create_exn ~name:"Test_circuit" [ output ] in
    let sim = Cyclesim.create circuit
    in Cyclesim.cycle sim;;
val run : Signal.t -> unit = <fun>
```
-->

# Cyclesim Coverage
[`Cyclesim`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/cyclesim_intf.ml) can
optionally capture coverage for one or multiple simulations of a
[`Circuit.t`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/circuit.mli).

## Enabling coverage
Coverage can be run over an executable or an `expect test` and coverage for `Circuit`s
that are structurally identical are merged and reported as a single unit.

### For an exe
To enable coverage reporting during an exe run, set the environment variable
`HARDCAML_CYCLESIM_COVERAGE=1`. This signals `Cyclesim` to collect coverage stats for each
simulation and generate a report in `coverage.txt` on program exit.


### For a single expect test
You can enable coverage tracking for a single expect test with the following lines at the
start and end of your testbench.

```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
(* Testbench code here *)
Cyclesim_coverage_expect_test.output_results ()
```

## Types of coverage
`Cyclesim` supports coverage on specific signal types, each of which are described
below. When coverage is enabled, coverage stats are collect for every signal of the kinds
that support coverage. Waivers for specific coverage cases can be added to specific
signals.

### Mux coverage
<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled;;
- : unit = ()
# let mux_selector = Signal.input "mux-selector" 2;;
val mux_selector : Signal.t = (wire (names (mux-selector)) (width 2))
# let mux = Signal.mux mux_selector [ const 1; const 2; const 3];;
val mux : Signal.t =
  (mux (width 2) (select mux-selector) (data (0b01 0b10 0b11)))
```
-->

Mux coverage tracks the values of the selector signal coming into the mux. Each possible
mux output must be selected once to get full coverage.

Sample coverage for a mux signal with three outputs whose selector is only ever 0 in sim:

<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run mux;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Mux with id: 2
    selector names: mux-selector
    saw selector values of: 0
    never saw: 1 2

- : unit = ()
```

#### Waiver
You can add a waiver for mux coverage with the following function:

```ocaml
# Signal.add_mux_waiver_exn mux (Waiver.exclude [1])
- : Signal.t = (mux (width 2) (select mux-selector) (data (0b01 0b10 0b11)))
```
<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run mux;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Mux with id: 2
    selector names: mux-selector
    saw selector values of: 0
    never saw: 2

- : unit = ()
```

### Case coverage
<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled;;
- : unit = ()
# let cases_selector = Signal.input "cases-selector" 2;;
val cases_selector : Signal.t = (wire (names (cases-selector)) (width 2))
# let cases = Signal.cases cases_selector ~default:(const 6) [ const 2, const 3; const 0, const 1; const 4, const 5];;
val cases : Signal.t =
  (cases (width 2) (select cases-selector)
 (cases ((0b10 0b11) (0b00 0b01) (0b00 0b01))) (default 0b10))
```
-->

Case coverage tracks which arm of the case is chosen each cycle. All arms must be selected
at least once to get full coverage.

Sample coverage for a cases signal with three arms and a default. In sim, only the arm in
position 1 is ever selected:

<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run cases;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Cases with id: 2
    selector names: cases-selector
    selector matched cases: 1
    never matched: 0 2 default

- : unit = ()
```

#### Waiver
You can add a waiver for cases coverage with the following function:

```ocaml
# Signal.add_cases_waiver_exn cases (Waiver.only_expect [Case.Positional 1; Default])
- : Signal.t =
(cases (width 2) (select cases-selector)
 (cases ((0b10 0b11) (0b00 0b01) (0b00 0b01))) (default 0b10))
```
<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run cases;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Cases with id: 2
    selector names: cases-selector
    selector matched cases: 1
    never matched: default

- : unit = ()
```

### Reg toggle coverage
<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled;;
- : unit = ()
# let reg_input = Signal.input "mux-selector" 4;;
val reg_input : Signal.t = (wire (names (mux-selector)) (width 4))
# let reg = Signal.reg (Signal.Reg_spec.create ~clock ()) reg_input;;
val reg : Signal.t =
  (register (width 4) ((clock clock) (clock_edge Rising))
 (data_in mux-selector))
```
-->

Register toggle coverage tracks which bits of a register are toggled. To get full coverage
every bit must flip from 0 -> 1 __and__ 1 -> 0.

Sample coverage for a 4 bit register that doesn't toggle any bits.
<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run reg;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Reg with id: 2
    width: 4
    bits never toggled ON: 0 1 2 3
    bits never toggled OFF: 0 1 2 3

- : unit = ()
```

#### Waiver
You can add a waiver for register coverage with the following function:

```ocaml
# Signal.add_register_waiver_exn reg (Waiver.only_expect [{Toggle.bit = 1; on = true}; {bit = 3; on = true};])
- : Signal.t =
(register (width 4) ((clock clock) (clock_edge Rising))
 (data_in mux-selector))
```
<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run reg;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Reg with id: 2
    width: 4
    bits never toggled ON: 1 3

- : unit = ()
```

### Always statemachine coverage
<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled;;
- : unit = ()
# module State = struct type t = A | B | C[@@deriving compare ~localize, enumerate, sexp_of, variants] end
module State :
  sig
    type t = A | B | C
    val compare__local : t @ local -> t @ local -> int
    val compare : t -> t -> int
    val all : t list
    val sexp_of_t : t -> Sexp_type.Sexp.t
    val a : t
    val b : t
    val c : t
    val is_a : t -> bool
    val is_b : t -> bool
    val is_c : t -> bool
    val a_val : t -> unit option
    val b_val : t -> unit option
    val c_val : t -> unit option
    module Variants :
      sig
        val a : t Base.Variant.t
        val b : t Base.Variant.t
        val c : t Base.Variant.t
        val fold :
          init:'a ->
          a:('a -> t Base.Variant.t -> 'b) ->
          b:('b -> t Base.Variant.t -> 'c) ->
          c:('c -> t Base.Variant.t -> 'd) -> 'd
        val iter :
          a:(t Base.Variant.t -> unit) ->
          b:(t Base.Variant.t -> unit) ->
          c:(t Base.Variant.t -> unit) -> unit
        val map :
          t ->
          a:(t Base.Variant.t -> 'a) ->
          b:(t Base.Variant.t -> 'a) -> c:(t Base.Variant.t -> 'a) -> 'a
        val make_matcher :
          a:(t Base.Variant.t -> 'a -> (unit -> 'b) * 'c) ->
          b:(t Base.Variant.t -> 'c -> (unit -> 'b) * 'd) ->
          c:(t Base.Variant.t -> 'd -> (unit -> 'b) * 'e) ->
          'a -> (t -> 'b) * 'e
        val to_rank : t @ local -> int
        val to_name : t -> string
        val descriptions : (string * int) list
      end
  end
# let sm = Always.State_machine.create (module State) (Signal.Reg_spec.create ~clock ()) ~enable:Signal.vdd;;
val sm : State.t Always.State_machine.t =
  {Hardcaml.Always.State_machine.current =
    (register (width 2) ((clock clock) (clock_edge Rising)) (data_in wire));
   is = <fun>; set_next = <fun>; switch = <fun>}
```
-->

Always statemachine coverage tracks the transitions that a `Always` DSL state register
makes. A set of validate transitions are computed from the compiled always code, and the
register must make each transition during sim to have full coverage.

Sample coverage for a register with three states (A,B,C):
```ocaml
# Always.compile [sm.switch [A, [sm.set_next B]; B, [sm.set_next C]; C, [sm.set_next A]]]
- : unit = ()
```
<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run sm.current;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Cases with id: 4
    always switch
        created at: <elided>
        driving (state variable): <elided>
    selector matched cases: A
    never matched: B C

Always state with id: 2
    never saw:
        A -> B
        B -> C
        C -> A

- : unit = ()
```

#### Waiver
You can add a waiver for statemachine coverage by either adding a state waiver or a state
transition waiver to the register signal. Waivers applied to statemachine state registers
carry over to switches performed on them.

Transition waiver:

```ocaml
# Signal.add_always_state_transition_waiver_exn sm.current (Waiver.exclude [ { Transition.from = "C"; to_ = "A"} ])
- : Signal.t =
(register (width 2) ((clock clock) (clock_edge Rising)) (data_in wire))
```
<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run sm.current;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Cases with id: 4
    always switch
        created at: <elided>
        driving (state variable): <elided>
    selector matched cases: A
    never matched: B C

Always state with id: 2
    never saw:
        A -> B
        B -> C

- : unit = ()
```

State waiver:

```ocaml
# Signal.add_always_state_waiver_exn sm.current (Waiver.exclude ["C"])
- : Signal.t =
(register (width 2) ((clock clock) (clock_edge Rising)) (data_in wire))
```
<!--
```ocaml
Cyclesim_coverage_expect_test.enable_and_maybe_reset ();
run sm.current;
```
-->
```ocaml
# Cyclesim_coverage_expect_test.output_compact_results ();
Cases with id: 4
    always switch
        created at: <elided>
        driving (state variable): <elided>
    selector matched cases: A
    never matched: B

Always state with id: 2
    never saw:
        A -> B

- : unit = ()
```
