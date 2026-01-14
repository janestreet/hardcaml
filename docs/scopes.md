# 5.6 Scopes

<!--
```ocaml
# open Base
# open Hardcaml
```
-->

A [`Scope.t`](https://github.com/janestreet/hardcaml/blob/with-extensions/src/scope.mli) is a
(mutable) object passed between the circuits that form a complete Hardcaml design. It
performs a number of roles

- Track the current position within a design hierarchy.
- Control how we elaborate a design.
- Provide a means to generate hierarchy aware names.
- Record sub-circuits within a database.
- Capture _side band_ data such as properties and assertions.

We will shortly learn how to generate hierarchical designs in Hardcaml, and `Scope`s play
a key role in that.

## Creating Scopes

```ocaml
# let scope = Scope.create ()
val scope : Scope.t = <abstr>
```

`Scope`s take a few interesting arguments which control how a design is elaborated.

- `flatten_design` if `true` the entire circuit is flattened (or inlined). If `false` it
  will retain hierarchy.
- `auto_label_hierarchical_ports` a useful option when using the Hardcaml waveform viewer
  that allows it to show the hierarchical structure of a design.
- `naming_scheme` control how names are generated. The defaults are set up to work with
  the value of `flatten_design`, but can be overridden.

### Flattening a Design

By default `Scope`s set `flatten_design` to `false`. This is generally what you want when
generating RTL. For simulation this wont work - our simulators do not support hierarchical
designs so this argument should be set to `true`:

```ocaml
# let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
val scope : Scope.t = <abstr>
```

### Sub-scopes

A new sub-scope can be created from a scope.  This is how we record a design hierarchy.

```ocaml
let scope = Scope.sub_scope scope "foo"
```

Sub-scopes are just `Scope`s placed at the appropriate point in the hierarchy.

## Naming signals with `Scope`s

We can define a scope aware naming function with:

```ocaml
# let (--) = Scope.naming scope
val ( -- ) : loc:[%call_pos] -> Signal.t -> string -> Signal.t = <fun>
```

And this is what we should do within hierarchical Hardcaml designs.

```ocaml
# let x = Signal.of_string "0" -- "x"
val x : Signal.t = (const (names (foo$x)) (width 1) (value 0b0))
```

Note that name of `x` is aware of its position within `foo`.

## Using Scopes

The main thing we need to do is pass a scope argument to every `create` function we
define:

```ocaml skip
let create (scope : Scope.t) (i : _ I.t) =
  let foo = Foo.create (Scope.sub_scope scope "foo") in
  ...
```

Sub-scopes are then created when we create other circuits. This is how they track the
current hierarchy position.

However, you shouldn't really ever need to actually do anything much with scopes except:

- Create them with `flatten_design` set to true or false depending on whether you want to
  generate RTL or run a simulation.
- Pass them to your create functions.

