# 2.4 RTL Generation

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
# open Base
# open Hardcaml
# open Signal
```
-->

You can [convert](https://github.com/janestreet/hardcaml/blob/with-extensions/src/rtl_intf.ml) a
Hardcaml [`Circuit`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/circuit.mli)
to either Verilog or VHDL.

The following is a trivial example.

```ocaml
let circuit = Circuit.create_exn ~name:"test" [ output "b" (input "a" 1) ]
```

```ocaml
# let () = Rtl.print Verilog circuit
module test (
    a,
    b
);

    input a;
    output b;

    assign b = a;

endmodule
```

```ocaml
# let () = Rtl.print Vhdl circuit
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port (
        a : in std_logic;
        b : out std_logic
    );
end entity;

architecture rtl of test is


begin

    b <= a;

end architecture;
```

We also provide basic Systemverilog support - the generated RTL is basically the same as
Verilog mode except we use the Systemverilog reserved words to correctly perform name
mangling.

# Instantiations

In Hardcaml, a `Circuit` corresponds to a single module in Verilog or entity in
VHDL. `Circuit`s can contain `Instantiation`s, which reference some other module or entity
within a hierarchical design.

The Hardcaml RTL generator is aware of the `Instantiation`s and can recursively generate
the RTL for them, if they are provided in a `Circuit_database`.

A `Circuit_database` stores the implementation of `Circuit`s that can be instantiated. It
is essentially a mapping between circuit names and their implementations.

If a circuit implementation is not found in the `Circuit_database`, Hardcaml will still
generate the appropriate instantiation in the RTL output. This allows for integration with
external modules (such as vendor IP) that will be added later in the design flow or for
[module hierarchy](module_hierarchy.md) to be described in Hardcaml.

# Printing Circuits

The `Rtl.print` function will output the RTL for a design (and optionally any
instantiations defined through a `Circuit_database`) to `stdout`.

It is simple and usable in most cases.

# Low Level Control

The RTL API provides a way for the Hardcaml to discover the full design hierarchy and for
the user to decide how to write it out.

It starts with the `create` function.

```ocaml
# Rtl.create
- : ?database:Hardcaml.Circuit_database.t ->
    ?config:Hardcaml__Rtl_config.t ->
    Rtl.Language.t -> Circuit.t list -> Rtl.Hierarchical_circuits.t list
= <fun>
```

- `database` - the `Circuit_database` which contains implementations of `Instantiations`
  found within the hierarchy of `Circuit`s.
- `config` - Some configuration options for how to generate the RTL.
- `Rtl.Langauge.t` - either Verilog or VHDL.
- `Circuit.t list` - a list of top-level circuits to generate.

`create` returns a list `Hierarchical_circuits` which represent the design hierarchy.

## Hierarchical circuits

The `Hierarchical_circuits` returned by `create` can be factored into the full design
hierarchy using `subcircuits` and `top_level_circuits`. These both return a list of
`Circuit_instance`s which contain functions to output a module implementation.

It should be noted that if a module is instantiated in multiple places it will still only
be represented once within the `Hierarchical_circuits` design hierarchy.

## Blackboxes

A blackbox is a module or entity which describes just its interface and does not include
its implementation. The RTL generator can create black boxes if required.

## Outputting RTL

Hardcaml predefines 4 ways to output the hierarchy of modules.

- `full_hierarchy` - generate full rtl for everything, recursively. This is the most common
  option and what `Rtl.print` uses.
- `top_levels_only` - only generate the given top levels and do not recuse into the
  hierarchy.
- `top_levels_and_blackboxes` - the top levels will be generated along with blackboxes for
  all modules in the hierarchy.
- `top_levels_as_blackboxes` - the top levels will be generated as blackboxes.

Each of these functions returns a `Rope.t`

### Ropes

Ropes are used by Hardcaml to generate code and are a fancy type of string. All you need
to know is they can be converted to a standard string with `Rope.to_string`.

## Example

```ocaml
let inner1 () = Circuit.create_exn ~name:"inner1" [output "b" (input "a" 1)]
let inner2 () =
    let x = input "x" 1 in
    let inst = Instantiation.create ~name:"inner1" ~inputs:["a", x] ~outputs:["b", 1] () in
    Circuit.create_exn ~name:"inner2" [output "y" (Instantiation.output inst "b")]
let top () =
    let s = input "s" 1 in
    let inst = Instantiation.create ~name:"inner2" ~inputs:["x", s] ~outputs:["y", 1] () in
    Circuit.create_exn ~name:"top" [output "t" (Instantiation.output inst "y")]
```

This creates a hierarchy where `top` instantiates `inner2` which in turn instantiates
`inner1`.

We now need to create a `Circuit_database` containing the inner circuits.

```ocaml
let database = Circuit_database.create ();;
Circuit_database.insert database (inner1 ());;
Circuit_database.insert database (inner2 ());;
```

Now we can print the RTL for `top` in various ways

```ocaml
let rtl = Rtl.create ~database Verilog [ top() ]
```

```ocaml
# Rtl.top_levels_and_blackboxes rtl |> Rope.to_string |> Stdio.print_endline;;
module inner1 (
    a,
    b
);

    input a;
    output b;


endmodule
module inner2 (
    x,
    y
);

    input x;
    output y;


endmodule
module top (
    s,
    t
);

    input s;
    output t;

    wire _4;
    wire _2;
    inner2
        the_inner2
        ( .x(s),
          .y(_4) );
    assign _2 = _4;
    assign t = _2;

endmodule

- : unit = ()
```

```ocaml
# Rtl.top_levels_as_blackboxes rtl |> Rope.to_string |> Stdio.print_endline;;
module top (
    s,
    t
);

    input s;
    output t;


endmodule

- : unit = ()
```

Find `inner2` within the hierarchy and print that directly.

```ocaml
# let inner2 =
    Rtl.Hierarchical_circuits.subcircuits rtl
    |> List.find_exn ~f:(fun sub -> String.equal (Rtl.Circuit_instance.module_name sub) "inner2")
    |> Rtl.Circuit_instance.rtl
    |> Rope.to_string
    |> Stdio.print_endline
module inner2 (
    x,
    y
);

    input x;
    output y;

    wire _4;
    wire _2;
    inner1
        the_inner1
        ( .a(x),
          .b(_4) );
    assign _2 = _4;
    assign y = _2;

endmodule

val inner2 : unit = ()
```
