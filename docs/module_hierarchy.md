# Module Hierarchies

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

*Prerequisite: [Hardcaml interfaces](hardcaml_interfaces.md)*

In Hardcaml, module
[hierarchy](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Hierarchy/index.html)
means we can partition a Hardcaml design
into a tree of sub-circuits and write each one to its own Verilog or
VHDL module when generating RTL.

This builds upon [instantiation](instantiation.md) and
[circuit databases](rtl_generation.md) by adding a new type called
`Scope.t` and a design pattern for defining circuits.

Module hierarchies provide the following benefits:

- Controls the naming of signals within the design hierarchy.
  - Timing reports/logic utilization reports from backend vendor
    tools will be much easier to comprehend.
  - Allows better organisation of waveforms which can group signals
    according to the module hierarchy.
- Reduces the size of the generated RTL file, where there is
  substantial logic duplication.

# Introduction to `Scope.t`

A [`Scope.t`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Scope/index.html)
is a (mutable) object passed between the circuits that
form a complete hardcaml design. It manages the current path in the
hierarchy of circuits and records unique circuits as they are
encountered in a circuit database.

# A design pattern for circuits

```ocaml
open Hardcaml

module type Module_design_pattern = sig
  module I : Interface.S
  module O : Interface.S

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
```

We define an input and output interface and a `create` function that
creates the hardware logic. We also pass it a scope argument. The
`hierarchical` function is mechanically derived from the `create`
function.

```ocaml
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; foo : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { foo_d : 'a [@bits 8] }
  [@@deriving hardcaml]
end
```

```ocaml
# let create (_scope : Scope.t) (input : Signal.t I.t) =
    let spec_with_clear =
      Reg_spec.create ~clock:input.clock ~clear:input.clear ()
    in
    let foo_d = Signal.reg spec_with_clear ~enable:Signal.vdd input.foo in
    { O. foo_d }
val create : Scope.t -> Reg_spec.signal I.t -> Reg_spec.signal O.t = <fun>

# let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"module_name" ~instance:"module_instance_2" create input
val hierarchical : Scope.t -> Reg_spec.signal I.t -> Reg_spec.signal O.t =
  <fun>
```

And that's it! When we want to instantiate this design as a
sub-circuit, just call `hierarchical` instead of `create`.

Compared to the non-hierarchical design flow all we need to do is

- Pass a [`Scope.t`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Scope/index.html) argument.
- Define the `hierarchical` function using the `Hierarchy.In_scope`
  functor.
- Call `hierarchical` when we instantiate it.

Hardcaml will detect when (structurally) equivalent circuits are
instantiated and generate a single Verilog (or VHDL) module for them.

# Naming Signals within Hierarchies

For naming to work properly with hierarchy, we must derive the names
from the `scope` argument.

```ocaml
# Scope.naming;;
- : ?sep:string -> Scope.t -> Reg_spec.signal -> string -> Reg_spec.signal =
<fun>
```

Here is an example.

```ocaml
let create (scope : Scope.t) (input : _ I.t) =
  (* A useful function alias for naming signals. *)
  let (--) = Scope.naming scope in
  let spec_with_clear =
    Reg_spec.create ~clock:input.clock ~clear:input.clear ()
  in
  let foo_d_next = Signal.(+:) input.foo input.foo -- "foo_d_next" in
  let foo_d = Signal.reg spec_with_clear ~enable:Signal.vdd foo_d_next in
  { O. foo_d }
;;
```

As opposed to using something like `Signal.( -- )`, `Scope.naming
scope` will append a path to the signal names as appropriate. This
ensures that the signal names do not end up cluttering the top level
of the waveform viewer.

# Flattening a hierarchical design

When we create a scope, we can choose how it should treat
sub-circuits. By default, a hierarchical design is created. If we pass
`flatten_design:true` then instead all sub-circuits will be inlined into
their parents.

## Simulation

For Hardcaml [simulation](simulation.md) to work, we need to flatten
the design.

```ocaml
# let create_sim () =
    let module Sim = Cyclesim.With_interface(I)(O) in
    let scope = Scope.create ~flatten_design:true () in
    Sim.create (create scope)
val create_sim : unit -> (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t = <fun>
```
