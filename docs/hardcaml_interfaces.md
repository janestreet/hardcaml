# Hardcaml Interfaces

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

Abstractly, Hardcaml
[interfaces](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Interface/index.html)
are made up of a polymorphic type and
a set of functions conforming to the module type `Interface.S`. The
type always represents a set of hardware signals, and the related
functions provide a way to label each element in the set with a name
and bit width.

More concretely, they are almost always made up of a record type where
each field corresponds to some hardware signal. This is then used to
describe the input and output ports of some hardware circuit.

[`ppx_hardcaml`](https://github.com/janestreet/ppx_hardcaml)
annotates an OCaml record that has type declarations
and automatically generates all the boilerplate code required for
interfaces. The record type should have a single polymorphic type, and
each field should be of that type.

Some simple [setup](installing_with_opam.md) is required to use
hardcaml interfaces, namely installing `ppx_hardcaml` and
adding it as a preprocessor.

```ocaml
module Simple_interface = struct
  type 'a t =
    { foo : 'a [@bits 32]
    ; (* Where a bit width is not specified, it defaults to 1. *)
      bar : 'a
    }
  [@@deriving hardcaml]
end
```

Notice that both `sexp_of` and `hardcaml` must be specified in the
`[@deriving]` specification.

The ppx supports nesting, arrays, lists and various tools to manage
port naming.

## Nesting

```ocaml
module Nested_interfaces = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; hello : 'a Simple_interface.t
    ; world : 'a Simple_interface.t
    }
  [@@deriving hardcaml]
end
```

Nesting can go as deep as required.

Enumerations are a special type of interface with a slightly different set of generated
functions but which can also be nested.

## Arrays and lists

For arrays and lists, the lengths must be specified with an attribute.

```ocaml
open Base
module Array_and_list_interfaces = struct
  type 'a t =
    { my_array : 'a array [@length 2]
    ; my_list : 'a list [@length 10] [@bits 10]
    }
  [@@deriving hardcaml]
end
```

Arrays of nested interfaces are also supported.

## Simple naming

By default, the field name is used to derive a port name. Port names
will be used during RTL generation. This can be modified with
attributes.

```ocaml
module Unmodified_port_names = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    ; c : 'a
    }
  [@@deriving hardcaml]
end
```

```ocaml
module Modified_port_names = struct
  type 'a t =
    { a : 'a[@rtlname "aaa"]
    ; b : 'a[@rtlprefix "x_"]
    ; c : 'a[@rtlsuffix "_x"]
    }
  [@@deriving hardcaml]
end
```

```ocaml
# Unmodified_port_names.port_names;;
- : string Unmodified_port_names.t =
{Unmodified_port_names.a = "a"; b = "b"; c = "c"}
# Modified_port_names.port_names;;
- : string Modified_port_names.t =
{Modified_port_names.a = "aaa"; b = "x_b"; c = "c_x"}
```

`rtlprefix`, `rtlsuffix` and `rtlmangle` attributes may also be used with nested
interfaces, though `rtlname` will not work.

## Global attributes

The naming attributes can be specified for all fields as follows:

```ocaml
module Global_suffix = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    ; c : 'a
    }
  [@@deriving hardcaml ~rtlsuffix:"_i"]
end
```
## Mangling

Mangling is used with nested interfaces.  Consider the nested example given previously.

```ocaml
# Nested_interfaces.port_names;;
- : string Nested_interfaces.t =
{Nested_interfaces.clock = "clock"; clear = "clear";
 hello = {Simple_interface.foo = "foo"; bar = "bar"};
 world = {Simple_interface.foo = "foo"; bar = "bar"}}
```

The port names of the nested fields for `hello` and `world` clash and
would eventually lead to a circuit error.

If we specify the rtlmangle option, the port names will be
differentiated by prefixing the field name from the outer interface.

```ocaml
module Nested_interfaces_mangled = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; hello : 'a Simple_interface.t
    ; world : 'a Simple_interface.t
    }
  [@@deriving hardcaml ~rtlmangle:true]
end
```

```ocaml
# Nested_interfaces_mangled.port_names;;
- : string Nested_interfaces_mangled.t =
{Nested_interfaces_mangled.clock = "clock"; clear = "clear";
 hello = {Simple_interface.foo = "hello_foo"; bar = "hello_bar"};
 world = {Simple_interface.foo = "world_foo"; bar = "world_bar"}}
```

## Comments

There are several benefits to using interfaces over working with raw
simulations:

- A nice [Simulation](simulation.md) that allows access to record
  fields directly instead of referring to port names via strings.
- Allows the creation of [module Hierarchies](module_hierarchy.md)
  within your RTL design.

There are currently some limitations on Hardcaml interfaces created
in this manner:

- They are unidirectional - groups of inputs or outputs need to be specified separately.
- When declaring an interface, the concrete bit widths must be known. Functorization is required
  if they need to be configurable.
