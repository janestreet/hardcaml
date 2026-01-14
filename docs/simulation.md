# 3.1 Simulating with Cyclesim

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

The simplest way to simulate a Hardcaml circuit is to use the
[`Cyclesim`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/cyclesim_intf.ml)
module.

This will take a [`Circuit.t`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/circuit.mli)
and perform various transformations in
order to produce an executable model of the design.

A testbench is used to control the design by:

1. Performing a reset
2. Stepping the simulation forward by 1 cycle
3. Setting inputs ports
4. Reading output ports

# Working with Simulators

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
val x : Bits.t ref = {Hardcaml__.Core0.contents = 0}
# let y = Cyclesim.out_port simulator "y"
val y : Bits.t ref = {Hardcaml__.Core0.contents = 0}
```

Note that looking up a non-existent port will produce an exception

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
[`Bits.t ref`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/bits_intf.ml).
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
# Bits.to_unsigned_int !y
- : int = 1
# Cyclesim.cycle simulator
- : unit = ()
# Bits.to_unsigned_int !y
- : int = 0
```


## The simulation algorithm

Cyclesim performs the following steps when `cycle` is called.

1. Based on the current value of the inputs, update (in the correct order) all the
   combinational nodes in the circuit. Nodes which read a register or memory read port use
   their current value.
2. Update all registers and memory writes. The previous step will have defined the new
   values to load.
3. Perform a final update of the (subset of) combinational nodes that depend on the new
   register/memory values.

### Before and After outputs

When we look up an output port there is an optional argument `clock_edge`. It defaults to
`After` which means the output value after all 3 steps of the simulation algorithm have
run. `Before` samples the values after step 1 - that is before the sequential nodes update.

# Limitations

Cyclesim implements a cycle-accurate simulation algorithm. This is in contrast to the
more complex event-driven simulators often used for RTL design.

Verilator is an example of a popular cycle-accurate simulator, while Icarus Verilog and
Modelsim are examples of event-driven simulators.

While event-driven simulators are more powerful, for testing standard synchronous designs
cycle accurate simulators are generally good enough, simpler to write testbenches for and
easier to make run reasonably fast.

The Hardcaml Cyclesim simulator also add some further restrictions:

- We only support 1 clock. In designs with more that 1 clock, they are all assumed to be
  the same.
- We don't model different clock edges - we assume everything is rising edge triggered.
- We only support 2 state logic.

The Hardcaml toolset provides an event driver simulator framework with the
`Hardcaml_event_driven_sim` library. This is rather more complex to use but provides more
accurate modeling possibilities.
