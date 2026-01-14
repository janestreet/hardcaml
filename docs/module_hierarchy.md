# 5.7 Module Hierarchies

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

*Prerequisite: [Hardcaml interfaces](hardcaml_interfaces.md)*

In Hardcaml, module
[hierarchy](https://github.com/janestreet/hardcaml/blob/with-extensions/src/hierarchy.mli) allows
us to partition a design into a tree of sub-circuits, each generated as its own Verilog or
VHDL module when producing RTL output.

This builds upon [instantiation](instantiation.md), [circuit
databases](rtl_generation.md), `Scope`s, and the design pattern for defining circuits
using `I` and `O` interfaces along with a `create` function.

Module hierarchies provide several important benefits:

- Controls the naming of signals within the design hierarchy.
  - Timing reports/logic utilization reports from backend vendor tools will be much easier
    to comprehend.
  - Allows better organization of waveforms which can group signals according to the
    module hierarchy.
- Reduces the size of the generated RTL file, where there is substantial logic
  duplication.
- Provides some options for controlling the backend process of building the final hardware
  design with vendor tools.

# A Design Pattern for Circuits

We extend our previous design pattern for circuits to now include a `Scope.t`.

```ocaml
open Hardcaml

module type Module_design_pattern = sig
  module I : Interface.S
  module O : Interface.S

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
```

The `I` and `O` interfaces and the `create` function we already know about.


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
      Signal.Reg_spec.create ~clock:input.clock ~clear:input.clear ()
    in
    let foo_d = Signal.reg spec_with_clear ~enable:Signal.vdd input.foo in
    { O. foo_d }
val create : Scope.t -> Signal.t I.t -> Signal.t O.t = <fun>
```

`hierarchical` is new, and is what we use to generate structured, multi-level Hardcaml
designs. It's mechanically derived as follows:

```ocaml
# let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope(I)(O) in
    H.hierarchical ~scope ~name:"module_name" ~instance:"module_instance_2" create input
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t = <fun>
```

The `hierarchical` function is built using the `Hierarchy.In_scope` functor. It provides a
function called `H.hierarchical` that takes `scope` and `create` and elaborates it
according to whether `flatten_design` setting of the root scope:

- **Flattened Mode** (`flatten_design = true`): The function calls the `create` function
  of your submodule directly, which inlines all the logic into the parent circuit. This
  results in a single, flat module with no hierarchy.

- **Hierarchical Mode** (`flatten_design = false`): The function generates an
  instantiation for our subcircuit, then elaborates it separately and stores its
  implementation in a `Circuit_database.t`. This maintains proper module hierarchy in the
  generated RTL.


# Summary

If we follow the design pattern here we must pass a scope to our `create` functions. We
then use the `Hierarchy.In_scope` functor to derive the `hierarchical` function. When we
want to instantiate a subcircuit, we call `hierarchical` instead of `create`.

To elaborate a full design, we call the top level `create` function with a scope that will
set `flatten_design` as appropriate. As the design is elaborated (by calling the
`hierarchical` functions of subcircuits) we will either build a single large fully inlined
design, or just the top level circuit along with a database containing all the
instantiated subcircuits.

## Simulation

<!--
```ocaml
let create (scope : Scope.t) (input : _ I.t) =
  (* A useful function alias for naming signals. *)
  let (--) = Scope.naming scope in
  let spec_with_clear =
    Signal.Reg_spec.create ~clock:input.clock ~clear:input.clear ()
  in
  let foo_d_next = Signal.(+:) input.foo input.foo -- "foo_d_next" in
  let foo_d = Signal.reg spec_with_clear ~enable:Signal.vdd foo_d_next in
  { O. foo_d }
;;
```
-->

Here's an example of how we would simulate a hierarchical design:

```ocaml
# let create_sim () =
    let module Sim = Cyclesim.With_interface(I)(O) in
    let scope = Scope.create ~flatten_design:true () in
    Sim.create (create scope)
val create_sim : unit -> (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t = <fun>
```

## RTL generation

For RTL generation we need to do a couple of things:

1. Create the scope with `flatten_design` set to false.
2. Construct a `Circuit`.
3. Get the circuit database from the scope.
4. Pass the circuit database to the `Rtl.print` function.

```ocaml
# let write_verilog () = 
    let module Circuit = Circuit.With_interface(I)(O) in 
    let scope = Scope.create ~flatten_design:false () in 
    let circuit = Circuit.create_exn ~name:"top" (create scope) in
    let database = Scope.circuit_database scope in 
    Rtl.print ~database Verilog circuit
val write_verilog : unit -> unit = <fun>
```
