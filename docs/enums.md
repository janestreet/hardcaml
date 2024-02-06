# Enums in Hardcaml

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

Hardcaml provides support for enumerations with a finite number of
instances through the Enum module. An Enum type declares a finite (ie
non-recursive) set of variants and requires a deriving specification
as follows.

```ocaml
module Simple_enum_example = struct
  module Enum = struct
    type t =
      | Foo
      | Bar
    [@@deriving sexp_of, compare, enumerate]
  end

  include Hardcaml.Enum.Make_enums(Enum)
end
```

The variant cases defining an enum may in turn have arguments which
are also Enums.

```ocaml
open Base
open Hardcaml

module Foo = struct
  type t =
    | Foo_a
    | Foo_b
  [@@deriving sexp_of, compare, enumerate]
end

module Bar = struct
  type t =
    | Bar_a
    | Bar_b
    | Bar_c
  [@@deriving sexp_of, compare, enumerate]
end

module Hello = struct
  module Enum = struct
    type t =
      | Foo of Foo.t
      | Bar of Bar.t
    [@@deriving sexp_of, compare, enumerate]
  end

  include Hardcaml.Enum.Make_enums(Enum)
end
```

The `Make_enums` functor creates two modules called `One_hot` and
`Binary`. Each of these modules conforms to the `Hardcaml.Interface.S`
signature so they can be treated as regular Hardcaml interfaces. The
`'a` parameter specifies the underlying type of the enum. ie: in
designing circuits, it would be `Signal.t`, and in simulations, it
will be `Bits.t ref`.

Both variants have a similar API, differing only in their encoding as
a bit vector. From here on, we will demonstrate the `Binary` API.

The generated modules contain `of_enum` functions to transform
statically known enums into arbitrary desired `'a t`, so long the
provided `'a` satisfied the `Comb.S` interface. (Both `(module Bits)`
and `(module Signal)` shipped with Hardcaml support them)

```ocaml
# let x : Signal.t Hello.Binary.t =
    Hello.Binary.of_enum (module Signal) (Foo Foo_a)
  ;;
val x : Reg_spec.signal Hello.Binary.t = <abstr>
# let y = Hello.Binary.Of_signal.of_enum (Bar Bar_a)
val y : Reg_spec.signal Hello.Binary.t = <abstr>

# let z = Hello.Binary.Of_signal.(==:) x y
val z : Reg_spec.signal = (const (width 1) (value 0b0))

# let a = Hello.Binary.Of_signal.is x (Foo Foo_a)
val a : Reg_spec.signal = (const (width 1) (value 0b1))
```

You get several enum-specific features for defining circuits in the
regular Signal-like API, notably the `match_` function for
multiplexing on the enum value. Note that this differs from `mux` which
creates a multiplexer that returns a value of the type enum.

```ocaml
let non_exhaustive_matching =
  let open Signal in
  Hello.Binary.match_
    (module Signal)
    (* [default] needs to be specified when the match cases are not
       exhaustive.
    *)
    ~default:(zero 10)
    x
    [ Foo Foo_a, ones 10
    ; Bar Bar_a, zero 10
    ]
;;

(* [Of_signal.match_] is similar to [match_], except it does not
   require an explicit a first class module argument.
*)
let exhaustive_matching =
  let open Signal in
  Hello.Binary.Of_signal.match_
    y
    [ Foo Foo_a, of_int ~width:10 55
    ; Foo Foo_b, of_int ~width:10 44
    ; Bar Bar_a, of_int ~width:10 66
    ; Bar Bar_b, of_int ~width:10 88
    ; Bar Bar_c, of_int ~width:10 77
    ]
;;
```

The `match_` function is also defined for the Always API, which makes
it really clean to write "pattern-match"-like applications on enums.
Similar to `Of_signal.match_`, there is an optional `default` argument
for non-exhaustive matches.

```ocaml
# Hello.Binary.Of_always.match_ ;;
- : ?default:Always.t list ->
    Reg_spec.signal Hello.Binary.t ->
    (Hello.Enum.t * Always.t list) list -> Always.t
= <fun>

# let exhaustive_matching case =
    Hello.Binary.Of_always.match_
      ~default:[
        (* The default case here. *)
      ]
      case
      [ Foo Foo_a, [
          (* Some logic here. *)
        ];
        Bar Bar_a, [
          (* Even more logic here *)
        ]
      ]
  ;;
val exhaustive_matching : Reg_spec.signal Hello.Binary.t -> Always.t = <fun>
```

This generated enum module implements the `Hardcaml.Interface.S`, so you
get access to the various convenient functions defined in `Of_signal`,
`Of_always` and `Of_bits`. For example:

```ocaml
(* A multiplexer that returns value of the enum, as opposed to
   multiplexing on the enum itself (as match_ does)
*)
let multiplexers =
  let selector = Signal.input "selector" 2 in
  Hello.Binary.Of_signal.(mux
    selector
    [ of_enum (Foo Foo_a)
    ; of_enum (Bar Bar_a)
    ; of_enum (Bar Bar_b)
    ; of_enum (Foo Foo_b)
    ]
  )
;;

let clock = Signal.input "clock" 1
let spec = Reg_spec.create ~clock ()

(* Registers that stores the enum value. *)
let registers : Signal.t Hello.Binary.t =
  let wires = Hello.Binary.Of_signal.wires () in

  Hello.Binary.Of_signal.reg ~enable:Signal.vdd spec wires
;;

(* Usage in always blocks. You can (almost) seamlessly assign values
   to them*)
let _ : Always.t list =
  let cond = Signal.input "cond" 1 in
  let var = Hello.Binary.Of_always.reg ~enable:Signal.vdd spec in
  let assign_hello = Hello.Binary.Of_always.assign in
  Always.[
    if_ cond [
      assign_hello var (Hello.Binary.Of_signal.of_enum (Foo Foo_a))
    ] @@ [
      (* This doesn't make much sense in practice. This is just
         to the [value] function.
      *)
      assign_hello var (Hello.Binary.Of_always.value var)
    ]
  ]
;;
```

The generated enum can even be used as part of regular hardcaml
interfaces! This makes Hardcaml Enums powerful, since the circuit
interfaces can simply use it without nasty type conversions. For
example:

```ocaml
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; hello : 'a Hello.Binary.t
    }
  [@@deriving hardcaml]
end
```

For more advanced usage, you can convert to the underlying `Bits.t` or
`Signal.t` value. The `of_raw` function only performs checks on
widths. It is the caller's responsibility to ensure that when it's
`One_hot`, the provided raw value is always one hot; when it's
`Binary`, the provided raw value should be strictly less than the
number of possible cases.

```ocaml
# let this_will_raise_due_to_a_width_mismatch =
    Hello.Binary.Of_signal.of_raw (Signal.of_int ~width:30 10)
  ;;
Exception: (Failure "Width mismatch. Enum expects 3, but obtained 30").

# let this_is_valid_and_fine =
    Hello.Binary.Of_signal.of_raw (Signal.of_int ~width:3 0)
  ;;
val this_is_valid_and_fine : Reg_spec.signal Hello.Binary.t = <abstr>

# let this_is_undefined_and_will_not_raise =
    Hello.Binary.Of_signal.of_raw (Signal.of_int ~width:3 6)
  ;;
val this_is_undefined_and_will_not_raise : Reg_spec.signal Hello.Binary.t =
  <abstr>
```

Enums are well supported in simulations too! Let's make a little state
machine that increments or decrements a counter based on an enum's input
value. The module simulation will also output the previous clock
cycle's enum input.

```ocaml

module O = struct
  type 'a t =
    { counter : 'a [@bits 32]
    ; prev_hello : 'a Hello.Binary.t [@rtlmangle true]
    }
  [@@deriving hardcaml]
end
```

```ocaml
# let create (i : _ I.t) =
    let open Signal in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let ctr = Always.Variable.reg spec ~width:32 ~enable:vdd in
    let prev_hello = Hello.Binary.Of_always.reg ~enable:vdd spec in
    Always.(compile [
      Hello.Binary.Of_always.match_ i.hello [
        (* Foo increments! *)
        Foo Foo_a, [ ctr <-- (ctr.value +:. 1); ];
        Foo Foo_b, [ ctr <-- (ctr.value +:. 2); ];

        (* Bar decrements.. *)
        Bar Bar_a, [ ctr <-- (ctr.value -:. 1) ];
        Bar Bar_b, [ ctr <-- (ctr.value -:. 2) ];
        Bar Bar_c, [ ctr <-- (ctr.value -:. 3) ];
      ];

      Hello.Binary.Of_always.assign prev_hello i.hello;
    ]);
    { O.
      counter = ctr.value
    ; prev_hello = Hello.Binary.Of_always.value prev_hello
    }
  ;;
val create : Reg_spec.signal I.t -> Reg_spec.signal O.t = <fun>
```

Since these enum files are not simply of type `Bits.t ref`, we need to
opt for special APIs for getting/setting them in simulations:

```ocaml
# Hello.Binary.sim_set
  ;;
- : Bits.t ref Hello.Binary.t -> Hello.Enum.t -> unit = <fun>

# Hello.Binary.sim_set_raw
  ;;
- : Bits.t ref Hello.Binary.t -> Bits.t -> unit = <fun>

# Hello.Binary.sim_get
  ;;
- : Bits.t ref Hello.Binary.t -> Hello.Enum.t Or_error.t = <fun>

# Hello.Binary.sim_get_raw
  ;;
- : Bits.t ref Hello.Binary.t -> Bits.t = <fun>
```

Using those functions, we can drive a hardcaml Cyclesim.t similar to
regular hardcaml sims:

```ocaml
let sim =
    let module Sim = Cyclesim.With_interface(I)(O) in
    Sim.create create
  ;;

let inputs = Cyclesim.inputs sim
;;

let outputs = Cyclesim.outputs sim
;;

let print () =
  let prev_hello = Or_error.ok_exn (Hello.Binary.sim_get outputs.prev_hello) in
  let counter = Bits.to_int !(outputs.counter) in
  Stdio.print_s [%message
    (prev_hello : Hello.Enum.t)
    (counter : int)
  ]
;;
```

```ocaml
# Hello.Binary.sim_set inputs.hello (Foo Foo_a);
- : unit = ()
# Cyclesim.cycle sim;
- : unit = ()
# print ();
((prev_hello (Foo Foo_a)) (counter 1))
- : unit = ()
# Hello.Binary.sim_set inputs.hello (Foo Foo_b);
- : unit = ()
# Cyclesim.cycle sim;
- : unit = ()
# print ();
((prev_hello (Foo Foo_b)) (counter 3))
- : unit = ()
# Hello.Binary.sim_set inputs.hello (Bar Bar_a);
- : unit = ()
# Cyclesim.cycle sim;
- : unit = ()
# Cyclesim.cycle sim;
- : unit = ()
# print ();
((prev_hello (Bar Bar_a)) (counter 1))
- : unit = ()
```
