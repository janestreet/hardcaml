# Naming

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

Names pop up in a couple of places. First, a circuit's input and output
ports must have properly defined names. Internal nodes within a circuit
also need a name which Hardcaml will automatically create unless provided.

Judicious use of names becomes very important for debugging
simulations or understanding reports from vendor tools like
Vivado.

# Port names

Port names are specified with the `input` and `output` functions.
Hardcaml checks the following rules:

- Port names must be unique.  No input or output may share a name.
- The names must be legal for the RTL language you use. For
  Hardcaml simulation, this doesn't matter, but if we write the
  design to Verilog, the names must not clash with a Verilog reserved
  word.
- Hardcaml will never try to alter a port name. Instead, it will raise
  an exception if it deems it illegal.

# Internal names

When we create vectors in Hardcaml, they are labelled with a unique ID.
Without further information, hardcaml will implicitly name the vector
as `_<uid>`. It is possible to manually label any vector with a new
name using the `(--)` operator.

```ocaml
# let foo = Hardcaml.Signal.(of_int ~width:8 7 -- "foo")
val foo : Hardcaml.Signal.t =
  (const (names (foo)) (width 8) (value 0b00000111))
```

There are no rules on what internal names it is OK to use. Hardcaml
will legalize them for you when generating RTL.  The rules for Verilog
generation are:

- Any non-alphanumeric character (except '$') is rewritten with an `_`.
- Internal names cannot start with a number - a prefix is added.
- Internal names cannot be reserved words - the name gets mangled.
- Internal names need to be unique - the name gets mangled if not.

Mangling means adding a numeric suffix and checking against the rules
again.

The VHDL rules are very similar.
