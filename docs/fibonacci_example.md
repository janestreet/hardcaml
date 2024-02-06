# Computing Finonacci numbers

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

In the following example we design a state machine which computes the
n-th Fibonacci number.

## Defining the circuit interfaces

```ocaml
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; n : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { done_ : 'a[@rtlname "done"]
    ; result : 'a[@bits 32]
    ; state : 'a[@bits 2]
    }
  [@@deriving hardcaml]
end
```

Note that we must use the field name `done_` in the output interface
as it is a reserved word in OCaml, but can use the ppx to give it our
preferred name.


## Defining the State type


```ocaml
open Base
open Hardcaml
module Waveform = Hardcaml_waveterm.Waveform

module States = struct
  type t =
    | S_wait
    | S_counting
    | S_write_result
  [@@deriving sexp_of, compare, enumerate]
end
```

The circuit will wait for a start signal in `S_wait` and capture the
input value `n` to compute the Fibonacci number for. The computation
will happen over multiple cycles in state `S_counting` and the result
will be output in the state `S_write_result`.

## Implementing the logic

```ocaml
# let create (i : _ I.t) =
    let open Signal in
    let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm =
      Always.State_machine.create (module States) ~enable:vdd r_sync
    in
    let done_ = Always.Variable.wire ~default:gnd in
    let result = Always.Variable.wire ~default:(zero 32) in
    let f0 = Always.Variable.reg ~width:32 ~enable:Signal.vdd r_sync in
    let f1 = Always.Variable.reg ~width:32 ~enable:Signal.vdd r_sync in
    let remaining = Always.Variable.reg ~width:8 ~enable:Signal.vdd r_sync in
    Always.(compile [
      sm.switch [
        S_wait, [
          (* the [a <--. b] is semantically equivalent to
              [a <-- Signal.of_int ~width:(Signal.width a.value) b].

              Similar syntatic sugar exists for [+:.] and [-:.]
          *)
          f0 <--. 1;
          f1 <--. 1;
          remaining <-- (i.n -:. 1);
          when_ i.start [
            if_ (i.n ==:. 0) [
              sm.set_next S_write_result;
            ] [
              sm.set_next S_counting;
            ]
          ]
        ];

        S_counting, [
          if_ (remaining.value ==:. 0) [
            sm.set_next S_write_result;
          ] [
            remaining <-- (remaining.value -:. 1);

            (* Recall that all hardcaml assignments are synchronous, so
                the [f1] assignment below will make use of the old [f0]
                value. *)
            f0 <-- f1.value;
            f1 <-- (f0.value +: f1.value);
          ]
        ];

        (* Output the computation result to the user. *)
        S_write_result, [
          done_ <--. 1;
          result <-- f1.value;
          sm.set_next S_wait;
        ]
      ]
    ]);
    { O.done_ = done_.value
    ; result = result.value
    ; state = sm.current
    (* We output the [state] to help tracing in the simulation example
       that follows. *)
    }
  ;;
val create : Reg_spec.signal I.t -> Reg_spec.signal O.t = <fun>
```

## Testing the logic

Lets now write a testbench which traces the sequence of states and the computed result.

```ocaml
# let fibonacci_testbench (sim : (_ I.t, _ O.t) Cyclesim.t) =
    let inputs, outputs = Cyclesim.inputs sim, Cyclesim.outputs sim in
    let print_state_and_outputs () =
      let state =
        List.nth_exn States.all (Bits.to_int !(outputs.state))
      in
      let done_ = Bits.is_vdd !(outputs.done_) in
      let result = Bits.to_int !(outputs.result) in
      Stdio.print_s [%message
        (state : States.t) (done_ : bool) (result : int)
      ]
    in

    (* Start by resetting simulation and clearing the circuit. *)
    Cyclesim.reset sim;
    inputs.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clear := Bits.gnd;

    (* Cycle 0 *)
    print_state_and_outputs ();

    (* Cycle 1 *)
    inputs.start := Bits.vdd;
    inputs.n := Bits.of_int ~width:8 4;
    Cyclesim.cycle sim;
    print_state_and_outputs ();
    inputs.start := Bits.gnd;

    (* Cycle 2 - Start counting the fibonacci number. *)
    Cyclesim.cycle sim;
    print_state_and_outputs ();

    (* Cycle 3 *)
    Cyclesim.cycle sim;
    print_state_and_outputs ();

    (* Cycle 4 *)
    Cyclesim.cycle sim;
    print_state_and_outputs ();

    (* Cycle 5 - notice that the wire assignment is combinational - the [done] signal
       is asserted during the same cycle it is assigned in the state machine.
    *)
    Cyclesim.cycle sim;
    print_state_and_outputs ();

    (* Cycle 6 - this goes back to the initial waiting state. *)
    Cyclesim.cycle sim;
    print_state_and_outputs ();

    Cyclesim.cycle sim
  ;;
val fibonacci_testbench : (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t -> unit =
  <fun>

# let waves =
    let module Sim = Cyclesim.With_interface(I)(O) in
    let sim = Sim.create create in
    let waves, sim = Waveform.create sim in
    fibonacci_testbench sim;
    waves
  ;;
((state S_wait) (done_ false) (result 0))
((state S_counting) (done_ false) (result 0))
((state S_counting) (done_ false) (result 0))
((state S_counting) (done_ false) (result 0))
((state S_counting) (done_ false) (result 0))
((state S_write_result) (done_ true) (result 5))
((state S_wait) (done_ false) (result 0))
val waves : Waveform.t = <abstr>
```

## Seeing the computation in a waveform

Lets set up some display rules so the waveform prints nicely.

A useful pattern when defining `display_rules` is to `map` over the
`port_names` and use the default `wave_format` `(Bit_or X)`. If the
width of the port is 1 it will show as a bit, otherwise, it will show
as the type `X`.

For the state value, we use the `Index` type which allows us to show
the value symbolically.

```ocaml
open Hardcaml_waveterm.Display_rule

let input_rules =
  I.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)) |> to_list)

let output_rules =
  O.(map port_names ~f:(port_name_is ~wave_format:(Bit_or Unsigned_int)))

let output_rules =
  { output_rules with O.state =
      port_name_is "state"
          ~wave_format:(Index (List.map States.all ~f:(fun t ->
                                 States.sexp_of_t t |> Sexp.to_string)))
  } |> O.to_list
```

Showing the waveform.

```ocaml
# let () =
    Waveform.print waves
      ~display_width:94
      ~display_rules:(input_rules @ output_rules)
  ;;
┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────┐
│clock             ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   │
│                  ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───│
│clear             ││        ┌───────┐                                                       │
│                  ││────────┘       └───────────────────────────────────────────────────────│
│start             ││                ┌───────┐                                               │
│                  ││────────────────┘       └───────────────────────────────────────────────│
│                  ││────────────────┬───────────────────────────────────────────────────────│
│n                 ││ 0              │4                                                      │
│                  ││────────────────┴───────────────────────────────────────────────────────│
│done              ││                                                        ┌───────┐       │
│                  ││────────────────────────────────────────────────────────┘       └───────│
│                  ││────────────────────────────────────────────────────────┬───────┬───────│
│result            ││ 0                                                      │5      │0      │
│                  ││────────────────────────────────────────────────────────┴───────┴───────│
│                  ││────────────────────────┬───────────────────────────────┬───────┬───────│
│state             ││ S_wait                 │S_counting                     │S_writ.│S_wait │
│                  ││────────────────────────┴───────────────────────────────┴───────┴───────│
│                  ││                                                                        │
└──────────────────┘└────────────────────────────────────────────────────────────────────────┘
```
