# 4.4 Instantiation

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

An
[instantiation](https://github.com/janestreet/hardcaml/blob/with-extensions/src/instantiation.mli)
creates a placeholder for a named sub-circuit with specified input and output ports.

This placeholder can be later filled in with an implementation -- it could be a Verilog
design, a vendor macro or even another Hardcaml circuit.

<!--
```ocaml
# open Base
# open Hardcaml
```
-->

```ocaml
# Instantiation.create;;
- : ?lib:string ->
    ?arch:string ->
    ?instance:string ->
    ?parameters:Hardcaml.Parameter.t list ->
    ?attributes:Hardcaml.Rtl_attribute.t list ->
    unit ->
    name:string ->
    inputs:(string * Signal.t) list ->
    outputs:(string * int) list -> Instantiation.t
= <fun>
```

Three arguments must be supplied

- `name` of the sub-circuit.
- `inputs` a list of port names and the signal they should attach to.
- `outputs` a list of port names and the width of the port.

The returned type can be queried using `Instantiation.output` to retrieve a signal for an
output port.

```ocaml
# let the_instantiation =
    let a = Signal.input "a_in" 2 in
    let b = Signal.input "b_in" 4 in
    Instantiation.create
      ~name:"my_sub_circuit_name"
      ~inputs:[ ("a", a) ; ("b", b) ]
      ~outputs:[ ("x", 5); ("y", 10) ]
      ()
val the_instantiation : Instantiation.t = <abstr>
# Instantiation.output the_instantiation "x"
- : Signal.t = (select (width 5) (range (4 0)) (data_in instantiation))
# Instantiation.output the_instantiation "y"
- : Signal.t = (select (width 10) (range (14 5)) (data_in instantiation))
```

`Instantiation.outputs` returns a map of output name to signal.

```ocaml
# Instantiation.outputs the_instantiation
- : Instantiation.output_map = <abstr>
```

## Parameters

External hardware designs are often configured using parameters (in Verilog) or generics
(in VHDL).

The optional `parameters` argument allows them to be specified. It takes a list of
`Parameter.t`s.

```ocaml
# let parameter = Parameter.create ~name:"bus_width" ~value:(Int 3)
val parameter : Parameter.t =
  {Hardcaml.Parameter.name = <abstr>; value = Hardcaml.Parameter.Value.Int 3}
```

Each parameter is specified using a `name` and a `type` with a value. Various
[types](https://github.com/janestreet/hardcaml/blob/with-extensions/src/parameter.mli) are
supported including `Int`, `String`, `Bool`, `Real` and various bit and vector types
associated with Verilog and VHDL - `Bit`, `Std_logic_vector` etc.

## Instantiation control

The optional `instance` parameter can provide a label for the specific instantiation of a
sub-circuit. Note that a sub-circuit can be instantiated multiple times and each one must
have a different label. Hardcaml will appropriately mangle labels so they are unique, but
a human can usually come up with better names.

`lib` and `arch` are concepts related to library management in VHDL. Hardcaml instantiates
in VHDL using the syntax

```
entity <lib>.module_name(<arch>)`
```

to avoid writing out component declarations. By default `work` and `rtl` are used.

## External IP

Instantiations can reference hardware designs written in another hardware description
language (HDL) such as Verilog or VHDL and connect them to a Hardcaml design.

Such designs contain a hole from Hardcaml's point of view. As such it will no longer be
possible to simulate them with `Cyclesim`.

They can be *linked* with the external design by an HDL simulator or the Vendor
implementation tools.

In some cases, you may be able to leverage
[`hardcaml_of_verilog`](http://github.com/janestreet/hardcaml_of_verilog) to convert a
Verilog design to a Hardcaml circuit which can be simulated.

## Hardcaml sub-circuits

We can also instantiate sub-circuits that we actually do have a Hardcaml implementation
of. This approach is used to describe [module hierarchies](module_hierarchy.md) in
Hardcaml.

Hardcaml can link such designs together allowing them to be simulated.
