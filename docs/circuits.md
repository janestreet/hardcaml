# 2.3 Circuits

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
# open Hardcaml
# open Signal
```
-->

A Hardcaml
[`Circuit.t`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/circuit.mli) encodes
both the RTL logic for a hardware design along with named input and output ports. Circuits
can be converted into various different forms:

* Verilog or VHDL
* Simulation models
* Verification models

# Creating a `Circuit`

Circuits are created from a list of outputs and a module name.

```ocaml
# let circuit =
    let foo = input "foo" 8 in
    let bar = input "bar" 8 in
    let baz = output "baz" (foo +: bar) in
    Circuit.create_exn ~name:"adder" [ baz ]
val circuit : Circuit.t = <abstr>
```

When generating Verilog or VHDL `name` will be used as the module or entity name in the
RTL design.

When generating a circuit, combinational loops are by default detected and will cause an
exception.

## Inputs and outputs

To create a circuit we must label the inputs and outputs.

Inputs are specified by giving a name and the required bit-width.

```ocaml skip
let input_a = Signal.input "a" width
```

Outputs are specified by giving a name and a signal.

```ocaml skip
let output_b = Signal.output "b" (some_logic input_a)
```

When generating Verilog or VHDL, inputs and outputs correspond to module (or entity)
ports.

For simulation, inputs are set by a testbench, and outputs are calculated by the
simulation models and read by a testbench.

The names given for inputs and outputs are special in that Hardcaml will never try to
change them. If it decides they are not valid for some reason an error is raised.

## What gets included

Circuits are defined by their output ports. Hardcaml traverses the design from the outputs
and discovers all internal nodes and input ports which are connected to the outputs.

This can sometimes be confusing - why is some part of my design not getting included? The
answer almost always is that it is not connected to an output.

## Config

There are a few configuration options used when generating circuits.

- `detect_combinational_loops` automatically check the circuit for combinational loops and
  raise if found.
- `normalize_uids` rewrite internal unique identifiers - helps to make the output rtl more
  stable to changes.

Both are true by default and generally should be kept as is.

There are a few other options but they are mainly used internally by Hardcaml.
