# 4.5 Working with Structural

<!--
```ocaml
# open Base
# let print_string = Stdio.print_string
val print_string : string @ local -> unit = <fun>
```
-->

# What is it?

Structural provides functions to generate hardware circuits. In fact its functionality is
very similar to `Hardcaml.Signal`, right down to supporting the standard Comb.S API.

That said, there are some difference with respect the `instantiation`s and `wire`s.

## Introducing tristates

Structural is not quite as silly as it seems. There is something that Hardcaml `Signal`s
do not support that we sometimes need - tristate values. A tristate value (also called a
three-state value) is a digital signal that can exist in one of three possible states:

- Logic high (1).
- Logic low (0).
- High impedance (Z).

First of all - why don't `Signal`s support tristates? They _could_ but it would make many
internal parts of the library much more complicated to do so. Indeed, all the backend
generators, simulators, and so on would need to be updated to support them, and we would
also have to use a multi-value logic type to do this properly.

So instead we provide `Structural` - a cut down version of `Signal` which supports
tristates, instantiation, writing to Verilog, and nothing else.

## Why do we need tristates?

Most of the time we don't. They are not generally used inside most logic designs. However,
when we interact with the outside world (ie via the pins on a chip) we quite often _DO_
need to support tristates. Some chips will use tristate signals to daisy chain multiple
devices or reduce pin count.

If we didn't provide a way to describe tristates the top level most module of a full
hardware design would still need to be described in a traditional HDL like Verilog. This
is what `Structural` is for - describing a design top level where we need support for
tristates.

# API

## Circuits

In structural, signals are recorded into a database rather than discovered dynamically as
with `Signal`s.

To do this we must first call `start_circuit name` and when done call `end_circuit ()`.

## Ports

- Input ports are created with `mk_input`
- Output ports are created with `mk_output`
- Tristate ports are created with `mk_tristate`

## Instantiation

Instantiations work a little differently than with `Signal`s. The inputs, outputs and
tristate ports of the instantiated circuit are all passed to the `inst` function.

## Basic example

```ocaml
# open Hardcaml.Structural
# let gen () =
    start_circuit "example";
    let i = mk_input "i" 1 in
    let o = mk_output "o" 1 in
    let t = mk_tristate "t" 1 in
    inst "inner" ~i:[ "i" ==> i ] ~o:[ "o" ==> o ] ~t:[ "t" ==> t ];
    end_circuit ();
    to_verilog (find_circuit "example") |> Rope.to_string |> print_string
val gen : unit -> unit = <fun>
# gen ()
module example
(
  input i,
  output o,
  inout t
);

  inner _4
  (
    .i(i),
    .o(o),
    .t(t)
  );
endmodule
- : unit = ()
```

Circuits must have unique names.

```ocaml
# gen ()
Exception: Hardcaml__Structural.Circuit_already_exists("example").
```

## API functions

A small set of functions are natively supported by `Structural` - `of_bit_string`, `mux`,
`concat_msb` and `select`.

In addition, assignment is performed with `<--`.

```ocaml
# let gen () =
    start_circuit "example2";
    let i = mk_input "i" 3 in
    let o = mk_output "o" 2 in
    o <--
        concat_msb
            [ of_bit_string "1"
            ; mux
                (select i ~high:0 ~low:0)
                [ select i ~high:1 ~low:1
                ; select i ~high:2 ~low:2 ]
            ];
    end_circuit ();
    to_verilog (find_circuit "example2") |> Rope.to_string |> print_string
val gen : unit -> unit = <fun>
# gen ()
module example2
(
  input [2:0] i,
  output [1:0] o
);

  wire _3;
  wire _4;
  wire _5;
  wire _6;
  wire _7;
  wire [1:0] _8;
  assign _3 = i[2:2];
  assign _4 = i[1:1];
  assign _5 = i[0:0];
  assign _6 =
    _5 == 0 ? _4 :
    _3;
  assign _7 = 1'b1;
  assign _8 = { _7, _6 };
  assign o = _8;
endmodule
- : unit = ()
```
### Using `Lib`

The complete `Comb` API be provided by instantiating the `Structural.Lib` functor (note
this must be done within a 'started' circuit). This uses hardcaml to create
implementations for each operation and allows them to be instantiated within `Structural`
circuits.

```ocaml
# let circuit =
    create_circuit "example3" (fun () ->
      let open Lib () in
      let a = mk_input "a" 3 in
      let b = mk_input "b" 3 in
      let addsub = mk_input "addsub" 1 in
      let c = mk_output "c" 3 in
      c <-- mux2 addsub (a +: b) (a -: b));;
val circuit : circuit = <abstr>
# to_verilog circuit |> Rope.to_string |> print_string
module example3
(
  input [2:0] a,
  input [2:0] b,
  input addsub,
  output [2:0] c
);

  wire _1;
  wire _2;
  wire [2:0] _7;
  wire [2:0] _9;
  wire [2:0] _11;
  assign _1 = 1'b1;
  assign _2 = 1'b0;
  assign _11 =
    addsub == 0 ? _7 :
    _9;
  assign c = _11;
  hardcaml_lib_sub_3 _8
  (
    .i0(a),
    .i1(b),
    .o(_7)
  );
  hardcaml_lib_add_3 _10
  (
    .i0(a),
    .i1(b),
    .o(_9)
  );
endmodule
- : unit = ()
```

We have used a new function here called `create_circuit`. It will automatically call
`start_circuit` and `end_circuit` and return the generated structural circuit.

The `(+:)` and `(-:)` functions have created instantiations for the operators. We can get
their implementations as follows:

```ocaml
# let components = structural_rtl_components circuit
val components : structural_rtl_component_set = <abstr>
# Set.iter components ~f:(fun subcircuit ->
    let subcircuit = Structural_rtl_component.rtl_circuit subcircuit in
    Hardcaml.Rtl.print Verilog subcircuit)
module hardcaml_lib_add_3 (
    i1,
    i0,
    o
);

    input [2:0] i1;
    input [2:0] i0;
    output [2:0] o;

    wire [2:0] _4;
    assign _4 = i0 + i1;
    assign o = _4;

endmodule
module hardcaml_lib_sub_3 (
    i1,
    i0,
    o
);

    input [2:0] i1;
    input [2:0] i0;
    output [2:0] o;

    wire [2:0] _4;
    assign _4 = i0 - i1;
    assign o = _4;

endmodule
- : unit = ()
```

## Describing a tristate mux

Tristate logic can also be generated by `Structural` rather than just passing through wires.

```ocaml
# let circuit =
    create_circuit "example4" (fun () ->
      let d = mk_input "d" 1 in
      let sel = mk_input "sel" 1 in
      let t = mk_tristate "t" 1 in
      t <-- mux sel [d; (z 1)])
val circuit : circuit = <abstr>
# to_verilog circuit |> Rope.to_string |> print_string
module example4
(
  input d,
  input sel,
  inout t
);

  wire _4;
  wire _5;
  assign _4 = 1'bz;
  assign _5 =
    sel == 0 ? d :
    _4;
  assign t = _5;
endmodule
- : unit = ()
```
