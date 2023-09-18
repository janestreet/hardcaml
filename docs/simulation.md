# Simulation

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

The simplest way to simulate a Hardcaml circuit is to use the
[`Cyclesim`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Cyclesim/index.html)
module.

This will take a [`Circuit.t`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Circuit/index.html)
and perform various transformations in
order to produce an executable model of the design.

A testbench is used to control the design by:

1. Performing a reset
2. Stepping the simulation forward by 1 cycle.
3. Setting inputs ports
4. Reading output ports

# Working with simulators

A simulation is built from a circuit as follows. First, we will create
a trivial circuit.

```ocaml
# open Base
# open Hardcaml
# let circuit = Circuit.create_exn ~name:"test" Signal.[ output "y" ~:(input "x" 1)]
val circuit : Circuit.t = <abstr>
```

Now we can build a simulator for this circuit.

```ocaml
# let (simulator : _ Cyclesim.t) = Cyclesim.create circuit
val simulator : (Cyclesim.Port_list.t, Cyclesim.Port_list.t) Cyclesim.t =
  <abstr>
```

Once we have the simulator we can query its input and output ports.
These functions use the input and output port names given when we
constructed the circuit.

```ocaml
# let x = Cyclesim.in_port simulator "x"
val x : Bits.t ref = {Base.Ref.contents = 0}
# let y = Cyclesim.out_port simulator "y"
val y : Bits.t ref = {Base.Ref.contents = 0}
```

Note that looking up a non-existant port will produce an exception

```ocaml
# let foo = Cyclesim.in_port simulator "foo"
Exception: ("Couldn't find input port" foo)
```

The functions `reset` and `cycle` control the simulator. `reset` sets
all registers to their default (initial) value. `cycle` reads the
values on the current input ports, updates the internal combinational
and sequential logic, and calculates the new values of the circuit
outputs.

```ocaml
# Cyclesim.reset simulator
- : unit = ()
# Cyclesim.cycle simulator
- : unit = ()
```

Input and output ports are of type
[`Bits.t ref`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Bits/index.html).
We set an input as follows:

```ocaml
# x := Bits.vdd
- : unit = ()
```

and read an output like so:

```ocaml
# !y
- : Bits.t = 1
# Bits.width !y
- : int = 1
# Bits.to_int !y
- : int = 1
```

# Visualizing Simulation Waveforms

See the pages on [waveforms](waveforms.md) and using the
[interactive viewer](waveterm_interactive_viewer.md) for more information.


# Limitations

The simulators provided by Hardcaml are called cycle-accurate
simulators, rather than the more complex event-driven simulators
often used for RTL design.

Verilator is an example of a popular cycle-accurate simulator, while
Icarus Verilog and Modelsim are examples of event-driven simulators.

While event-driven simulators are more powerful, for testing standard
synchronous designs cycle accurate simulators are generally good
enough, simpler to write testbenches for and easier to make run
reasonably fast.

The Hardcaml simulators also add some further restrictions -
specifically it is possible to (approximately) model multiple clock
domains with cycle-accurate simulators, but we do not currently allow
that. Further, we do not model different clock edges and assume all
synchronous logic is triggered on the rising edge.

Let us know if this is particularly restrictive for you!
