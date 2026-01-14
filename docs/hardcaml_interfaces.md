# 5.1 Hardcaml Interfaces

<!--
```ocaml
# open Base
# open Hardcaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

Abstractly, Hardcaml
[interfaces](https://github.com/janestreet/hardcaml/blob/with-extensions/src/interface_intf.ml) are
made up of a polymorphic type (with a single polymorphic variable) and a set of functions
which can manipulate that type:

```
type 'a t [@@deriving sexp_of]

val iter : 'a t -> f:('a -> unit) @ local -> unit
val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) @ local -> unit
val map : 'a t -> f:('a -> 'b) @ local -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) @ local -> 'c t
val to_list : 'a t -> 'a list
```

In addition they contain a field

```
  val port_names_and_widths : (string * int) t
```

which specifies for each `'a` contained in `'a t` a string name which can be used to label
the value and the bit width of it's representation as hardware.

Most commonly an interface will be of type `Signal.t t` where it will represent a set of
Hardcaml signals within a hardware design, or `Bits.t t` where it is used to interact with
a Hardcaml simulator.

Interfaces actually contain many more convenience functions though they can all be
derived from the functions shown above.

## Record interfaces

The most common form of interface is a record. Below we implement an interface by hand for
a record containing two fields: `foo` and `bar`.

```ocaml
module Explicit_interface_record_implementation = struct
  module T = struct
    type 'a t =
      { foo : 'a
      ; bar : 'a
      }
    [@@deriving equal ~localize, compare ~localize, sexp_of]

    let map t ~f = { foo = f t.foo; bar = f t.bar }
    let map2 s t ~f = { foo = f s.foo t.foo; bar = f s.bar t.bar }
    let iter t ~f = f t.foo; f t.bar
    let iter2 s t ~f = f s.foo t.foo; f s.bar t.bar
    let to_list t = [ t.foo; t.bar ]

    let port_names_and_widths = { foo = "FOO_FOO", 32; bar = "BAR_BAR", 1 }
  end
  include T
  include Hardcaml.Interface.Make(T)
end;;
```

In the `port_names_and_widths` value we gave a 32 bit width to foo, and a 1 bit width to
bar. Note also the names we specified were not the same as the record field name (although
much of the time it makes most sense if they are).

To reduce the amount of boilerplate code to write we provide a ppx called
[`ppx_hardcaml`](https://github.com/janestreet/ppx_hardcaml) which can generate the above
code for you from the type definition.

The type definition derives Hardcaml and various annotations exist to customize the names
and widths of each field.  Note that each field in the record must be of type `'a`.

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

Some simple [setup](installing_with_opam.md) is required to use Hardcaml interfaces,
namely installing `ppx_hardcaml` and adding it as a preprocessor in the build system.

## Using interfaces

Lets say `Simple_interface` defined the outputs of some hardware module. Generally we
would write something like:

<!---
```ocaml
# open Signal
# let outputs = Simple_interface.Of_signal.of_unsigned_int 0
val outputs : t Simple_interface.t =
  {Simple_interface.foo = (const (width 32) (value 0x00000000));
   bar = (const (width 1) (value 0b0))}
```
-->

```ocaml
let circuit =
    Circuit.create_exn ~name:"test"
      [ output "foo" outputs.foo; output "bar" outputs.bar ]
```

Instead we can write:

```ocaml
let circuit =
    Circuit.create_exn ~name:"test"
      Simple_interface.(map2 port_names outputs ~f:Signal.output |> to_list)
```

Now lets say we have built a simulator over this module and want access to the output ports

<!--
```ocaml
# let sim =
  let module Sim = Cyclesim.With_interface(Interface.Empty)(Simple_interface) in
  Sim.create (fun _ -> outputs)
val sim :
  (Bits.t ref Interface.Empty.t, Bits.t ref Simple_interface.t) Cyclesim.t =
  <abstr>
```
-->

```ocaml
let outputs =
    Simple_interface.(map port_names ~f:(Cyclesim.out_port sim ))
```

## Other types of interface

There are various use cases for interfaces where the outer type is not a record. `Enums`
are one example and will be described later.

`Scalar`s are another where we abstract a Hardcaml value by restricting access to it's
implementation. They often come with a specialized API for manipulating the value.

# Complete Interface API

<!--
```ocaml
# open Simple_interface
```
-->

## Map and Iter

`map` and `iter` functions are provided with up to 5 arguments.

```ocaml
# map5
- : 'a Simple_interface.t ->
    'b Simple_interface.t ->
    'c Simple_interface.t ->
    'd Simple_interface.t ->
    'e Simple_interface.t ->
    f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) @ local -> 'f Simple_interface.t
= <fun>
```

The f arguments have a local mode annotation. Modes like local apply only to Oxcaml users.
OCaml users can ignore the local annotation.

The local mode indicates values that cannot escape their scope. Values are global by
default unless annotated otherwise. Functions expecting a local argument can accept either
local or global values, but functions expecting a global argument can only accept global
values. See the [OxCaml documentation on
+modes](https://oxcaml.org/documentation/modes/intro/) for more information.

Interface users typically don't need to consider locality. This mainly matters when
defining interfaces manually (not via the ppx). Some tips for manual interface
definitions:

- Explicitly annotate the f argument with @ local rather than relying on mode inference.
  This annotation is required in module signatures, as omitting it implies global mode.
- When a function's tail call passes f to another function, annotate the tail call with
  [@nontail]. This is necessary because the compiler optimizes tail calls for constant space
  by default. See the oxcaml documentation on stack allocation for details on why nontail is
  needed.

## Zip

Between 2 and 5 interfaces can be combined using zip.  The result is an interface of tuples.

```ocaml
# zip port_names port_widths
- : (string * int) Simple_interface.t = {foo = ("foo", 32); bar = ("bar", 1)}
```

## Fold and Scan

`fold` passes each field of an interface, along with an accumulator, to the given function
`f`. For example we can compute the total width of an interface as follows.

```ocaml
# fold port_widths ~init:0 ~f:(fun total width -> total + width)
- : int = 33
```

(note - this is also provided by the value `sum_of_port_widths`).

`scan` is similar except it returns an interface. The function `f` returns the next value
of the accumulator and the value of the result field. We can compute the offset of each
field in an interface as follows.

```ocaml
# scan port_widths ~init:0 ~f:(fun acc width -> acc + width, acc)
- : int Simple_interface.t = {foo = 0; bar = 32}
```

(note - this is also provide by the function `offsets`).

`fold2` and `scan2` are also defined.

## Association lists and Tags

An interface can be converted to and from an association list with `to_alist` and
`of_alist`. Note that we do not choose to use the string name of fields as keys - rather
we define and abstract type `tag` and value `tags` which uniquely represent each fields in
an interface.

## Lists of Interfaces

`of_interface_list` and `to_interface_list` convert between a list of interfaces and
interface of lists.  The signatures should make the operation clearer.

```ocaml
# of_interface_list
- : 'a Simple_interface.t list -> 'a list Simple_interface.t = <fun>
# to_interface_list
- : 'a list Simple_interface.t -> 'a Simple_interface.t list = <fun>
```

## Misc functions

`port_names  : string t` and `port_widths : int t` provide direct access to the names and widths of fields in an interface.

`const c` sets each field to the value `c`.

## Of\_signal and Of\_bits

Both these modules implement the signature `Interface.Comb`. They provides functions
specialized to the types `Signal.t t` and `Bits.t t` respectively.

### Converting from Ints

`of_unsigned_int`, `of_signed_int` and `of_int_trunc` set each field to the given value by
converting from a given integer.

`of_unsigned_ints`, `of_signed_ints` and `of_ints_trunc` also convert from integers but each
field may be specified individually.

### Pack and Unpack

`pack` flattens an interface into a single vector by concatenating all the fields.  `unpack` reverses the operation.

### Muxs

`mux2` selects between 2 interfaces and `mux` selects between an arbitrary number of interfaces.

### Selection

`priority_select`, `priority_select_with_default` and `onehot_select` all work the same
way as the corresponding versions on normal signals, expect they operate on all fields at
once.


### Of\_signal specifics

A few further functions apply only to the `Signal.t t` type.

`wires` creates an interface of wires.

`assign a b` performs wire assignment - `a` should be an interface of wires.
`reg` and `pipeline` apply registers (or a pipeline of registers) to the given interface.

`inputs` and `outputs` create the input and output ports for circuits.

`apply_names` will apply names to each field based on `port_names` and a given prefix and suffix.


## Of\_always

`Of_always` operates on interfaces of type `Always.Variable.t t`.

`value` converts an interface of variables to an interface of signals.

`assign` assigns variables with values. It results in an always statement.

`reg` and `wire` define interfaces of variables.

`apply_names` will apply names to each field based on `port_names` and a given prefix and suffix.
