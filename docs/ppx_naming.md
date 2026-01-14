# 5.8 Naming with ppx_hardcaml

<!--
```ocaml
# open Base
# open Hardcaml
# open Signal
```

-->
# Naming with `ppx_hardcaml`

We have seen the `(--)` operator which applies a simple string name to a signal. We have
also seen `Scope.naming` which creates a naming operator appropriate for the current level
in a design hierarchy.

The Hardcaml ppx provides a syntax which makes it very convenient to apply scoped names to
Hardcaml signals.

## Basic use

```ocaml
# let create scope x =
  let%hw y = ~: x in
  y
val create : Scope.t -> t -> t = <fun>
# create (Scope.create ()) (of_string "111")
- : t = (const (names (y)) (width 3) (value 0b000))
```

The syntax `%hw` can be applied to let bindings whose name will then be applied to the
expression. A `scope` must be (excuse me for this) _in scope_ for the ppx to work.

An additional syntax allows the naming of `Always.Variable`s.

```ocaml
# let create scope =
    let%hw_var x = Always.Variable.wire ~default:gnd () in
    x.value;;
val create : Scope.t -> t = <fun>
# create (Scope.create ())
- : t = (wire (names (x)) (width 1))
```

## Naming interfaces

Interfaces can be named with the ppx:

<!--
```ocaml
module X = struct type 'a t = { x : 'a }[@@deriving hardcaml] end;;
```
-->

```ocaml
# let create scope x =
    let%hw.X.Of_signal foo = x in
    foo
val create : Scope.t -> X.Of_signal.t -> X.Of_signal.t = <fun>
# create (Scope.create ()) { X.x = (of_string "111") }
- : X.Of_signal.t = {X.x = (const (names (foo$x)) (width 3) (value 0b111))}
```

Similarly, interfaces containing `Always.Variable`s are named thus:

```ocaml
# let create scope x =
    let%hw.X.Of_always foo = X.Of_always.wire Signal.zero in
    X.Of_always.value foo
val create : Scope.t -> 'a -> t X.t = <fun>
# create (Scope.create ()) { X.x = (of_string "111") }
- : t X.t = {X.x = (wire (names (foo$x)) (width 1))}
```

## Naming with a module path

Interface naming is actually done via a more general framework. When we write
`let%hw.Module.Path` the ppx finds a function called `Module.Path.__ppx_auto_name`. This
is given the thing to name and a prefix derived from the scope and binding name and
sets the names.

This can be applied to specialized Hardcaml types and in particular is implemented for
`Always.State_machine`.

<!--
```ocaml
# module State = struct type t = A [@@deriving sexp_of, compare ~localize, enumerate] end;;
module State :
  sig
    type t = A
    val sexp_of_t : t -> Sexp.t
    val compare__local : t @ local -> t @ local -> int
    val compare : t -> t -> int
    val all : t list
  end
# let spec = Reg_spec.create ~clock:gnd ()
val spec : Reg_spec.t = <abstr>
```
-->

```ocaml
# let create scope =
    let%hw.Always.State_machine my_state_machine = Always.State_machine.create (module State) spec in
    my_state_machine
val create : Scope.t -> State.t Always.State_machine.t = <fun>
# create (Scope.create ())
- : State.t Always.State_machine.t =
{Hardcaml.Always.State_machine.current =
  (register (names (my_state_machine)) (width 1)
 ((clock 0b0) (clock_edge Rising)) (data_in wire));
 is = <fun>; set_next = <fun>; switch = <fun>}
```

# Tuples

Bindings which describe a tuple are also supported.

```ocaml
# let create scope x =
  let%hw y, z = ~: x, x +:. 1 in
  y, z
val create : Scope.t -> t -> t * t = <fun>
# create (Scope.create ()) (of_string "111")
- : t * t =
((const (names (y)) (width 3) (value 0b000)),
 (const (names (z)) (width 3) (value 0b000)))
```

We can use `%hw_var` and `%hw.Module.Path` with this form also, however, all components
must be of the appropriate, and same, type (ie `Signal.t` or `Always.Variable.t` or
`Some_interface.t`).

# Lists, Arrays, and Iarrays

The final supported form allows lists, arrays, and iarrays to be named. As might be
expected, it applies the name along with an index for each element.

```ocaml
# let create scope x =
  let%hw_list foo = x in
  foo
val create : Scope.t -> t list -> t list = <fun>
# create (Scope.create ()) [ of_string "111"; of_string "101" ]
- : t list =
[(const (names (foo$0)) (width 3) (value 0b111));
 (const (names (foo$1)) (width 3) (value 0b101))]
```

`%hw_list` is used for lists of signals.  `%hw_array` and `%hw_iarray` is similarly for
arrays and iarrays respectively.

`%hw_var_list`, `%hw_var_array` and `%hw_var_iarray` expect the list/array/iarray to
contain `Always.Variable.t`s.

These forms may also include a module path for use with interfaces and so on.
