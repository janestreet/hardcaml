# Simulating with interfaces

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

With standard Hardcaml [simulations](simulation.md) we are forced to manage input and
output ports manually using the
strings. [Interfaces](hardcaml_interfaces.md) allow us to automate
much of this work and are particularly useful as circuits get more
complex.

The following is a common pattern for specifying a hardware circuit.
It consists of an input and output interface, and a function over
these interfaces which constructs the logic.

```ocaml
open Base
open Hardcaml

(* Input interface. *)
module I = struct
  type 'a t =
     { clock : 'a
     ; foo : 'a [@bits 8]
     ; bar : 'a [@bits 8]
     }
  [@@deriving hardcaml]
end

(* Output interface. *)
module O = struct
  type 'a t =
    { baz : 'a [@bits 8]
    ; baz_delayed : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

let create (i : Signal.t I.t) : Signal.t O.t =
  let open Signal in
  let spec = Reg_spec.create ~clock:i.clock () in
  let baz = i.foo +: i.bar in
  let baz_delayed = reg ~enable:vdd spec baz in
  { O. baz ; baz_delayed }
;;
```

To simulate this, we can use the
[`Cyclesim.With_interface`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Cyclesim/With_interface/index.html)
functor. This will automatically create a circuit with the input and output
ports labelled, build a simulator, and then construct input and output
records for driving the simulator. All this and we never have to worry
about the underlying string names of ports.

```ocaml
# let create_sim () =
    let module Sim = Cyclesim.With_interface(I)(O) in
    Sim.create create
  ;;
val create_sim : unit -> (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t = <fun>
```

Notice the type signature of `(_, _) Cyclesim.t`. The parametric type
arguments to [`Cyclesim.t`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Cyclesim/index.html)
encode the types returned when retrieving the
inputs and output values from the simulator object.

Driving the inputs and reading outputs can be performed via the
convenience of record fields.

```ocaml
# let run_sim () =
    let sim = create_sim () in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in
    let print_outputs () =
      Stdio.print_s (
        [%sexp_of: int O.t]
          (O.map outputs ~f:(fun p -> Bits.to_int !p)))
    in

    inputs.foo := Bits.of_int ~width:8 1;
    inputs.bar := Bits.of_int ~width:8 2;
    Cyclesim.cycle sim;
    print_outputs ();

    inputs.foo := Bits.of_int ~width:8 7;
    inputs.bar := Bits.of_int ~width:8 9;
    Cyclesim.cycle sim;
    print_outputs ();
  ;;
val run_sim : unit -> unit = <fun>

# run_sim ()
((baz 3) (baz_delayed 3))
((baz 16) (baz_delayed 16))
- : unit = ()
```
