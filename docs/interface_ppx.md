# 5.2 Interfaces with ppx_hardcaml

<!--
```ocaml
open Base
open Stdio
```
-->

# Interfaces with ppx_hardcaml

```ocaml
module Simple_interface = struct
  type 'a t =
    { foo : 'a [@bits 32]
    ; bar : 'a (* Where a bit width is not specified, it defaults to 1. *)
    }
  [@@deriving hardcaml]
end
```

```ocaml
# print_s [%message (Simple_interface.port_names_and_widths : (string * int) Simple_interface.t)]
(Simple_interface.port_names_and_widths ((foo (foo 32)) (bar (bar 1))))
- : unit = ()
```

By default the ppx uses the field name for the string name, and `[@bits N]` attribute to
specify the field width - if not provided it defaults to `1`.

The ppx supports nesting, arrays, lists, and various tools to manage port naming.

## Nesting

Interfaces can contain other interfaces:

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

## Arrays and lists

For `array`s and `list`s, the lengths must be specified with an attribute.

```ocaml
module Array_and_list_interfaces = struct
  type 'a t =
    { my_array : 'a array [@length 2]
    ; my_list : 'a list [@length 10] [@bits 10]
    }
  [@@deriving hardcaml]
end
```

Arrays of nested interfaces are also supported.

## Options

Fields in an interface may be optional. Simple and nested fields are supported.

```ocaml
module Interface_with_option = struct 
  type 'a t = 
    { maybe_x : 'a option[@exists true][@bits 3] 
    ; maybe_y : 'a Simple_interface.t option [@exists false]
    }
    [@@deriving hardcaml]
end
```

The attribute `exists` controls if they are included in the interface or not and must be
specified. Such fields can arise when describing a circuit generator where some
configuration option may require extra inputs or outputs.

# Naming

## Naming simple fields

By default, the field name is used to derive a port name. Port names will be used during
RTL generation. This can be modified with attributes.

- `rtlname` overrides the fields name with the given string.
- `rtlprefix` / `rtlsuffix` apply the given prefix and/or suffix to the name

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

## Naming nested interfaces

`rtlprefix` and `rtlsuffix` can be applied to nested interfaces and will apply the given
prefix or suffix to each field within the nested interface.

By default, we automatically mangle nested interface names by prefixing them with the
field name and a `$` separator, to avoid name clashes.

```ocaml
# Nested_interfaces.port_names;;
- : string Nested_interfaces.t =
{Nested_interfaces.clock = "clock"; clear = "clear";
 hello = {Simple_interface.foo = "hello$foo"; bar = "hello$bar"};
 world = {Simple_interface.foo = "world$foo"; bar = "world$bar"}}
```

This functionality can be disabled with `~rtlmangle:false`, or customized by specifying a
different separator.

```ocaml
module Nested_interfaces_mangled = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; hello : 'a Simple_interface.t
    ; world : 'a Simple_interface.t
    }
  [@@deriving hardcaml ~rtlmangle:"_"]
end
```

```ocaml
# Nested_interfaces_mangled.port_names;;
- : string Nested_interfaces_mangled.t =
{Nested_interfaces_mangled.clock = "clock"; clear = "clear";
 hello = {Simple_interface.foo = "hello_foo"; bar = "hello_bar"};
 world = {Simple_interface.foo = "world_foo"; bar = "world_bar"}}
```

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

`rtlprefix`, `rtlsuffix` and `rtlmangle` are supported.
