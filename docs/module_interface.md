# 5.3 Module Interfaces

<!--
```ocaml
# open Base 
# open Hardcaml
# open Signal
```
-->

# Module Interfaces

The primary use of interfaces is to define a general pattern to describe a Hardcaml
module. We will specify two interfaces called `I` and `O` which represent the input and
output signals of a module along with a function called `create` to generate the logic.

```
module I : Interface.S
module O : Interface.S

val create : Signal.t I.t -> Signal.t O.t
```

Hardcaml provides various functors (usually called `With_interface`) that will generate
circuits, simulators, hierarchical instantiations, and more if we follow this pattern.

> 📝 The type of the create function does not have to precisely follow this signature. It
> is fine to have further arguments (i.e. some configuration parameters) so long as it
> ends with `Signal.t I.t -> Signal.t O.t`.
 
## Example

```ocaml
module I = struct
  type 'a t =
    { clock : 'a
    ; d : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { q : 'a[@bits 8]
    ; q_n : 'a[@bits 8]
    }
  [@@deriving hardcaml]
end
```

```ocaml
# let create (i : _ I.t) =
    let q = reg (Reg_spec.create ~clock:i.clock ()) i.d in
    { O.q; q_n = ~: q }
val create : t I.t -> t O.t = <fun>
```

## `With_interface`

Hardcaml supports this pattern by providing a number of functors usually called
`With_interface`. They take the `I` and `O` interfaces as parameters and will provide a
function which takes `create`.

For example, we can create a circuit as follows:

```ocaml
# module My_circuit = Circuit.With_interface(I)(O)
module My_circuit :
  sig
    type create = Hardcaml.Interface.Create_fn(I)(O).t
    val create_exn :
      ?config:Circuit.Config.t ->
      ?input_attributes:Rtl_attribute.t list I.t ->
      ?output_attributes:Rtl_attribute.t list O.t ->
      name:string -> create -> Circuit.t
  end
# My_circuit.create_exn ~name:"reg" create
- : Circuit.t = <abstr>
```

In contrast this is what we would write without interfaces:

```ocaml
# let create2 ~clock ~d = 
    let q = reg (Reg_spec.create ~clock ()) d in 
    q, ~:q
val create2 : clock:t -> d:t -> t * t = <fun>
# let q, q_n = create2 ~clock:(input "clock" 1) ~d:(input "d" 8) 
val q : t =
  (register (width 8) ((clock clock) (clock_edge Rising)) (data_in d))
val q_n : t = (not (width 8) (arguments (register)))
# Circuit.create_exn ~name:"reg"
    [ output "q" q; output "q_n" q_n ]
- : Circuit.t = <abstr>
```

As the number of signals into and out of a module grows (and 100's or even 1000's is not
infeasible) the utility of interfaces to deal with boilerplate grows.

## Configuration

When we define an interface we specify things like bit widths, field existence (using
options) and array or list lengths. However, we often want to write something more
generic. To do this we can use functors.

Taking the previous example, lets make the register width configurable.

```ocaml
module Make(Config : sig val register_width : int end) = struct 
  module I = struct 
    type 'a t = 
      { clock : 'a 
      ; d : 'a[@bits Config.register_width] 
      }
    [@@deriving hardcaml]
  end

  module O = struct 
    type 'a t = 
      { q : 'a[@bits Config.register_width] 
      ; q_n : 'a[@bits Config.register_width]
      }
    [@@deriving hardcaml]
  end

  let create (i : _ I.t) = 
      let q = reg (Reg_spec.create ~clock:i.clock ()) i.d in
      { O.q; q_n = ~: q }
  end
```

We can now instantiate the functor and generate a circuit as before.

```ocaml
module My_design = Make(struct let register_width = 4 end) 
module My_circuit2 = Circuit.With_interface(My_design.I)(My_design.O)
```

```ocaml
# My_circuit2.create_exn ~name:"reg" My_design.create
- : Circuit.t = <abstr>
```

### Interface type (in-)equality

Consider the following 2 instantiations of our design.

```ocaml
module My_design16 = Make(struct let register_width = 16 end)
module My_design32 = Make(struct let register_width = 32 end)
```

Inside each instantiation we have records of type `I.t` and `O.t`. They have exactly the
same definition so are the types equal?

No. Because they are created inside a functor, distinct types are generated.

On the other hand would we like them to be equal? I would argue no. Because they represent
interfaces with different bitwidths I would like the type system to differentiate them.

On occasion it is useful to expose the type equality and it can still be done.

```ocaml
module X = struct 
  type 'a t = { x : 'a }[@@deriving hardcaml]
end

module Make_X(Config : sig val width : int end) 
  : Interface.S with type 'a t = 'a X.t
= struct 
  include Interface.Update(X)(struct 
    let port_names_and_widths = { X.x = "x", Config.width }
  end)
end
```

`Interface.Update` takes an existing interface and redefines the names and bitwidths of
the fields. The type constraint `with type 'a t = 'a X.t` exposes the desired equality.

```ocaml
module Y = Make_X(struct let width = 10 end)
module Z = Make_X(struct let width = 20 end)
```

As shown below the types of `Y.t` and `Z.t` are compatible.

```ocaml
# [ Y.port_names; Z.port_names ]
- : string Z.t list = [{X.x = "x"}; {X.x = "x"}]
```
