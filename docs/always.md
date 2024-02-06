# Always DSL

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

The [`Always` DSL](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Always/index.html)
in Hardcaml lets one describe circuits
in a similar style to a Verilog `always` block. Always blocks allow
hardware multiplexer structures to be described in a somewhat
imperative manner using variable assignments, if-else conditions,
switches and a simple procedural macro construct.

This programming model often makes complicated logic (such as
state machines) easier to reason about.

# The Always DSL

```ocaml
open Base
open Hardcaml
```

There are two key components when using the Always DSL:

## 1. Variable Declarations

There are two kinds of variable declarations, namely wires and
registers.

A `reg` is one whose value is sequentially updated on the edge of the
clock signal provided within a `Reg_spec.t` type.

```ocaml
# (* Creates a register variable. *)
  Always.Variable.reg
- : ?enable:Signal.t ->
    width:int -> Signal.Type.register -> Always.Variable.t
= <fun>
```

A `wire` is one whose value is updated combinatorially, meaning that
the new value is visible at the same clock cycle as when it is
assigned. The new value _will not_ persist to the next cycle. If no
such assignment exists, the variable will possess the `default` value.

```ocaml
# (* Creates a wire register, that is, the value of the wire *)
  Always.Variable.wire
- : default:Reg_spec.signal -> Always.Variable.t = <fun>
```

Both kinds of variable will return the same type, namely an `Always.Variable.t`

```ocaml
# let foo = Always.Variable.wire ~default:Signal.gnd ;;
val foo : Always.Variable.t =
  {Hardcaml.Always.Variable.value = (wire (width 1) (data_in empty));
   internal = <abstr>}
```

Variables may be assigned within an Always block. To read the value
of a variable, access the `value` field, which provides a `Signal.t`
that can be used to form expressions.

```ocaml
# foo.value;;
- : Reg_spec.signal = (wire (width 1) (data_in empty))
```

## 2. Writing an Always Program


Having declared the variables, we can now write the actual
'procedural' blocks.

An `Always` program comprises assignments to variables optionally
guarded by `if_`, `when` or `switch` conditional statements. For example:

```ocaml
let something =
  let open Signal in
  let a = input "a" 1 in
  let b = input "b" 1 in
  let c = Always.Variable.wire ~default:gnd in
  let d = Always.Variable.wire ~default:gnd in
  let e = Always.Variable.wire ~default:gnd in
  Always.(compile [
    (* Assignments. *)
    c <-- (a ^: b );

    (* [if_] statements. *)
    if_ (a ==: b) [
      d <-- vdd;
    ] [
      d <-- gnd;
    ];

    (* [when_] is like [if_], with an empty [else] *)
    when_ c.value [
      e <--. 1;
    ];
  ]);
;;

```

Notice the call to the `compile` function surrounding the procedural
block.

```ocaml
# Always.compile
- : Always.t list -> unit = <fun>
```

Two things going on here:

- Notice that `compile` returns a unit. This is because variable
  declarations create unassigned wires, and the compile function assigns them
  to the appropriate multiplexers.
- Notice that the always DSL is just a list of `Always.t`. This
  means we can play with various metaprogramming and abstraction
  tricks within these program blocks.

## Example

The following is an example of these pieces put together.

```ocaml
let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let a = Signal.input "a" 8
let b = Signal.input "b" 8
let r_sync = Reg_spec.create ~clock ~clear ()

let create =
  let open Signal in
  (* [wire] and [register] variable declarations. *)
  let c_wire = Always.Variable.wire ~default:(Signal.zero 8) in
  let c_reg = Always.Variable.reg ~enable:Signal.vdd r_sync ~width:8 in
  (* The program block with a call to [compile] *)
  Always.(compile [
    if_ (a ==: b) [
      c_wire <-- (sll a 1);
      c_reg  <-- (sll a 1)
    ] [
      c_wire <-- (a +: b);
      c_reg  <-- (a +: b);
    ]
  ]);
  (* the [c_wire.value] are assigned appropriately by the Always
  compiler. *)
  output "c_wire" c_wire.value, output "c_reg" c_reg.value
;;
```

## Differences to Verilog Always blocks

The semantics are similar to Verilog, with a few caveats:

- Assignments are non-blocking. Hardcaml does not support blocking
  assignments.
- In Verilog, an always block can describe either registers or
  combinational logic, but not both. With the Always DSL, both can be
  defined in a single block.
- Hardcaml always blocks do not require a sensitivity list - the clocks
  driving the registers are bound to the register variable themselves.

## How values update

The reader might be surprised by the following example.

```ocaml
let counter_thing enable =
  let open Signal in
  let a = Always.Variable.reg ~enable:vdd r_sync ~width:8 in
  Always.(compile [
    (* Here we might expect [a] to take the values [10] or [11], depending on
       [enable].  But that is not the case.  While [enable] is high, [a] will
       increment by one every clock cycle.  When [enable] is low [a] is set to
       10.  *)
    a <--. 10;
    when_ (enable) [
      a <-- a.value +:. 1;
    ]
  ]);
;;
```

The rule is: the last assignment executed will set the next value.
