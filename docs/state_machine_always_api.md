# Designing State Machines with the Always DSL

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

# Writing State Machines

The [Always DSL](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Always/index.html)
is most commonly used to construct state machines or
non-trivial sequential logic. Hardcaml is shipped with an
[`Always.State_machine`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Always/State_machine/index.html)
module to help create state machines.

A state machine is constructed with the following function:

```ocaml
# open Base
# open Hardcaml
# open Signal
# Always.State_machine.create
- : ?encoding:Always.State_machine.Encoding.t ->
    ?auto_wave_format:bool ->
    ?enable:t ->
    (module Hardcaml.Always.State_machine.State with type t = 'a) ->
    Type.register -> 'a Always.State_machine.t
= <fun>
```

The value returned can be used within an *Always block*.
Let's look at an example.

## Defining the State type

```ocaml
module States = struct
  type t =
    | Wait_for_start
    | Process_something
    | Process_something_else
  [@@deriving sexp_of, compare, enumerate]
end
```

The `States` type must use the `sexp_of`, `compare` and `enumerate` derivers
when defining a state machine.

This state machine will begin in the state `Wait_for_start` and wait
for an external `start` signal. It will then transition to
`Process_something`, `Process_something_else` and back to
`Wait_for_start`. Just before transitioning back to `Wait_for_start`
it will pulse a done signal.

Although nothing useful is actually implemented here, we could imagine
the processing states were waiting for network data, accessing a
RAM, or performing some multi-step computation.

## Implementing the state machine

```ocaml
let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let r_sync = Reg_spec.create ~clock ~clear ()
let start = Signal.input "start" 1

let outputs =
  let open Signal in
  let sm =
    Always.State_machine.create (module States) ~enable:vdd r_sync
  in
  let done_ = Always.Variable.wire ~default:gnd in
  Always.(compile [
    sm.switch [
      Wait_for_start, [
        when_ start [
          sm.set_next Process_something;
        ]
      ];

      Process_something, [
        sm.set_next Process_something_else
      ];

      Process_something_else, [
        done_ <--. 1;
        sm.set_next Wait_for_start;
      ]
    ]
  ]);
  [ Signal.output "done" done_.value
  ; (* We output the [state] to help with visualizing in the
        simulation examples that follows. *)
    Signal.output "state" sm.current
  ]
;;
```

Notice the `Always.compile` function call surrounding the Always
DSL. Let's look at its type signature:

```ocaml
# Always.compile
- : Always.t list -> unit = <fun>
```

What? It returns a unit type, so how do we retrieve the variable's
values? Under the hood, the always DSL creates regular Hardcaml wires
with an empty assignment. The call to `Always.compile` will assign
these wires with the appropriate signals according to the conditions
specified in the always block. For more information about wires, see
the [Sequential Logic](sequential_logic.md) section. In particular,
unassigned wires are fairly common if you miss a variable
assignment.

## Simulation

[Simulating](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Cyclesim/index.html)
a state machine is no different from simulating any other
Hardcaml circuit. Let's walk through an example to see the
Always DSL in action.

```ocaml
# let () =
    let circuit = Circuit.create_exn ~name:"test_statemachine" outputs in
    let sim = Cyclesim.create circuit in
    let print_state_and_outputs () =
      let state =
        List.nth_exn States.all (Bits.to_int !(Cyclesim.out_port sim "state"))
      in
      let done_ = Bits.is_vdd !(Cyclesim.out_port sim "done") in
      Stdio.print_s [%message
        (state : States.t) (done_ : bool)
      ]
    in

    Cyclesim.reset sim;
    Cyclesim.in_port sim "clear" := Bits.vdd;
    Cyclesim.cycle sim;
    print_state_and_outputs ();
    Cyclesim.in_port sim "clear" := Bits.gnd;

    Cyclesim.in_port sim "start" := Bits.vdd;
    Cyclesim.cycle sim;
    print_state_and_outputs ();
    Cyclesim.in_port sim "start" := Bits.gnd;

    Cyclesim.cycle sim;
    print_state_and_outputs ();

    Cyclesim.cycle sim;
    print_state_and_outputs ()
  ;;
((state Wait_for_start) (done_ false))
((state Process_something) (done_ false))
((state Process_something_else) (done_ true))
((state Wait_for_start) (done_ false))
```

# Metaprogramming with the Always DSL

As mentioned above, the
[Always DSL](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Always/index.html)
is simply an `Always.t list`. The gives room for several creative behaviours.

## Function Abstractions

Since we are really just generating `Always.t` lists from OCaml code,
we can simply split out some parts of the DSL into different
functions. For example:

```ocaml
(* A useful mental model above is to treat `foo_branch` as a "C void
   function" that takes a pointer to write its output value to.
*)
let foo_branch (o_value : Hardcaml.Always.Variable.t) =
  let open Signal in
  let foo = Always.Variable.reg ~enable:vdd  ~width:32 r_sync in
  Always.[
    foo <-- (foo.value +:. 1);
    o_value <-- foo.value;
  ]
;;

let main : Always.t list =
  let cond = Signal.input "cond" 1 in
  let o_value = Always.Variable.wire ~default:(Signal.zero 32) in
  Always.[
    if_ cond [
      (* [proc] turns a [Always.t list] to an [Always.t], without
         changing any semantic meaning of the program.
      *)
      proc (foo_branch o_value);
    ] [
      o_value <--. 0;
    ]
  ]
;;
```

A few interesting things are happening here:

- We can call `foo_branch` as many times as we want.
- Every call to `foo_branch` creates a new instance of an accumulator
  register.
- The caller doesn't know (and doesn't care) that `foo_branch` created
  a new variable under the hood. This is akin to function closures in
  functional programming, a powerful concept made possible in the Always DSL.

Bearing in mind that we are still generating hardware (so all
variables in functions are really more like "static variables" in C), this
"function abstraction" is a powerful way of making repetitive /
complicated state machines much more comprehensible.

## (Advanced) "High-order" Blocks

What if we want to create functional blocks that are only executed
under a set of non-trivial preconditions?

For example - processing data received from some hand-shaking
protocol. In this example, we consider a trivial case where only an
`accept` signal is required.


```ocaml
type stream =
  { valid : Signal.t
  ; data : Signal.t
  ; accept : Always.Variable.t
  }

let handshake stream callback =
  Always.(proc [
    when_ stream.valid [
      stream.accept <--. 1;
      proc (callback stream.data);
    ]
  ])
;;

let main (stream_a : stream) (stream_b : stream) =
  let foo = Always.Variable.wire ~default:Signal.gnd in
  let bar = Always.Variable.wire ~default:Signal.gnd in
  Always.compile [
    handshake stream_a (fun data ->
      Always.[
        foo <-- data
      ]
    );

    handshake stream_b (fun data ->
      Always.[
        foo <--. 0;
        bar <-- ~:(data)
      ]
    );
  ]
;;
```

The beauty in this approach is that the repetitive
hand-shaking code is replaced with a high order function call. On top
of that if the handshaking protocol is modified, we can simply update
the `handshake` function without having to rewrite it everywhere it is
used.
