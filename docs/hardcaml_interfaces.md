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
[interfaces](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Interface/index.html) are
made up of a polymorphic type (with a single polymorphic variable) and a set of functions
which can manipulate that type:

```
type 'a t [@@deriving sexp_of]

val iter : 'a t -> f:('a -> unit) -> unit
val iter2 : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
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
    [@@deriving sexp_of]

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
