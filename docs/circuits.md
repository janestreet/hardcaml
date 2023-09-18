# Circuits

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

A Hardcaml [`Circuit.t`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Circuit/index.html)
encodes both the RTL logic for a hardware
design along with named input and output ports. Circuits can be
converted into various different forms:

* Verilog or VHDL
* Simulation models
* Verification models

# Creating a `Circuit`

Circuits are created from a list of outputs and a module name.

```ocaml
# open Hardcaml
# let circuit =
    let open Signal in
    let foo = Signal.input "foo" 8 in
    let bar = Signal.input "bar" 8 in
    let baz = Signal.output "baz" (foo +: bar) in
    Circuit.create_exn ~name:"adder" [ baz ]
val circuit : Circuit.t = <abstr>
```

When generating Verilog or VHDL the module name will be used in the
RTL design.

The list of outputs is recursively searched to find all circuit
inputs, and thus includes all logic between the given outputs and
found inputs.

When generating a circuit, combinational loops are by default detected
and will cause an exception.

# Inputs and outputs

To create a circuit we must label the inputs and outputs.

Inputs are specified by giving a name and the required bit-width.

```ocaml skip
let input_a = Signal.input "a" width
```

Outputs are specified by giving a name and a signal.

```ocaml skip
let output_b = Signal.output "b" (some_logic input_a)
```

When generating Verilog or VHDL, inputs and outputs correspond to
module (or entity) ports.

For simulation, inputs are set by a testbench, and outputs are
calculated by the simulation models and read by a testbench.

# Instantiation

Circuits may instantiate other circuits. Within a Hardcaml design, one
may write

```ocaml
# open Base
# open Hardcaml
# let an_instantiation =
   Instantiation.create ()
     ~name:"some_subcircuit"
     ~inputs:[ "a", Signal.vdd; "b", Signal.of_string "111" ]
     ~outputs:[ "c", 8 ]
val an_instantiation : Signal.t Base.Map.M(Base.String).t = <abstr>
# Map.find_exn an_instantiation "c"
- : Signal.t = (wire (width 8) (data_in instantiation))
```

The given name corresponds to the module name of the instantiated
circuit. `inputs` provide a port name and the signal to which it is
connected. `outputs` also provide a port name and must specify the
width of the output port. The returned value is a map that can be
queried to access the output ports.

Sub-circuits can be Hardcaml circuits or written in some other RTL
design language like Verilog.

In later (advanced) sections we will show how to use
circuits and [instantiations](instantiation.md)
to support [hierarchical](module_hierarchy.md) Hardcaml designs.
