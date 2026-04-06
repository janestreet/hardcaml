# FAQ

# FAQ

> What is an unassigned wire error when I create my circuit?

```
    "circuit input signal must have a port name (unassigned wire?)"
```

A circuit can be thought of as a directed, cyclic graph. It has inputs, outputs and a
bunch of internal nodes.

Hardcaml traverses your circuit starting at the outputs and tracing back through the
internal nodes until it finds the inputs.

If you have an unassigned wire in your graph, this will look like an input node, but it
won't be properly named. Thus hardcaml raises an exception.

In order to trace where the problem is, you can set:

```
let () = Caller_id.set_mode Top_of_stack
```

somewhere before you construct your design. The exception should now include the location
at which the unassigned wire was created.

> It looks like parts of my circuit are missing?  What happened?

When Hardcaml traverses your circuit, it can only find nodes that are actually connected
to one of the outputs.

If a node is not attached to an output, it will not be included in the circuit.

> I am pretty sure my logic is connected to an output, but it's still missing?

It's possible the Hardcaml optimizer worked out that the logic wasn't used and got rid of it.

As an example, suppose you have:

```
open! Hardcaml
open! Signal

let x = vdd |: a in
```

Hardcaml will realize that x is always `vdd` and remove the expression.

You can choose to bypass these optimizations with the `Signal.Unoptimized` module, which
shadows some operators from `Signal` to disable optimization:

```
open! Hardcaml
open! Signal
open! Signal.Unoptimized

let x = vdd |: a in
```

> What is a combinational loop?

A combinational loop is a cycle of combinational nodes. In standard digital design flows
the only cycles that are allowed must pass through a sequential element (i.e. register or
memory).

They are detected by default when a circuit is created and an exception is thrown. The
exception includes the nodes present in the combinational loop. You can set the
`Caller_id` mode to add more debug information.

```
    ("Combinational loop" (
      (or (width 1) (arguments (a wire)))
      (wire
        (width   1)
        (data_in or))))
```

You can disable this check with:

```
Circuit.create_exn
  ~config:{ Circuit.Config.default with detect_combinational_loops = false }
```

> Why does (a +: b) raise an exception?

Hardcaml enforces certain rules on the arguments of operators. In this case both `a` and
`b` must be the same width.

Without these rules we need to understand how the arguments are resized or their
signed-ness interpreted. Instead we provide modules like `Comb.Signed` or `Comb.Unsigned`
which have an appropriate set of rules for its type (for example,
`Signal.Unsigned.(a +: b)` will choose an appropriate output width such that the result
cannot overflow).

> I can only see the input and output ports of my circuit in simulation

By default, only the top level ports are traced in Hardcaml simulations.

You can view any internal node, but you need to tell the simulator which ones to include.
Typically with `Cyclesim` (and related simulators) this is done by adding the following
config argument:

```
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all ...
```

Tracing a lot of nodes does slow down the simulation a bit.

> What are the keybindings for the Waveterm viewer?

Type `?` and it'll tell you!

You can also rebind them all with a `.hardcamlwavetermrc` file.
