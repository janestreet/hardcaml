# High performance simulation backends

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

The default simulator shipped with Hardcaml is a cycle-accurate
simulator implemented in OCaml. There are two other types of simulator
backends shipped with Hardcaml, available in the `hardcaml_c` and
`hardcaml_verilator` libraries.

The libraries can be installed with opam.

```
opam install hardcaml_verilator
opam install hardcaml_c
```

# `Hardcaml_verilator`

[Verilator](https://www.veripool.org/wiki/verilator) is a free and
open-source software tool which converts Verilog to a cycle-accurate
behavioural model in C++ or SystemC that runs with high
performance. `Hardcaml_verilator` is a simulation backend that
compiles a Hardcaml circuit simulation to the Verilator backend, with
custom-generated C++ bindings. All these are done under the hood
whilst exposing the Cyclesim API to the end user.

To use [`Hardcaml_verilator`](https://ocaml.org/p/hardcaml_verilator/latest/doc/Hardcaml_verilator/index.html),
[Verilator](https://www.veripool.org/wiki/verilator) and g++ have to be
installed on your system.

The key benefit of using the Verilator-based backend is that it is
much more performant than using Cyclesim or Hardcaml_c.

There are several known downsides with this simulation backend:

- The Verilator simulator does not support inspecting the values of
  internal signals of the circuit.
- Compiling extremely large circuits on Verilator can be slow, but the
  trade-off is worthwhile for large-scale simulations.

```ocaml
open Hardcaml
open Signal

(* Circuit definition. *)
let clock = input "clock" 1
let foo = input "foo" 32
let bar = input "bar" 32
let baz =
  let r_sync = Reg_spec.create ~clock () in
  output "baz" (reg ~enable:vdd r_sync (foo +: bar))
;;

(* Create Simulation. *)
let circuit = Circuit.create_exn ~name:"adder" [ baz ]

```

After creating the Verilator-based Cyclesim object, the simulation
works as would any cyclesim simulator.

```ocaml
# let cycle sim foo bar =
    (Cyclesim.in_port sim "foo") := Bits.of_int ~width:32 foo;
    (Cyclesim.in_port sim "bar") := Bits.of_int ~width:32 bar;
    Cyclesim.cycle sim;
    Stdio.printf "%d + %d = %d\n"
      foo bar (Bits.to_int !(Cyclesim.out_port sim "baz"));
  ;;
val cycle : ('a, 'b) Cyclesim.t -> int -> int -> unit = <fun>

# let sim_verilator =
    Hardcaml_verilator.create
       ~clock_names:[ "clock" ]
       circuit
  ;;
val sim_verilator : Cyclesim.t_port_list = <abstr>

#  cycle sim_verilator 1 2;
1 + 2 = 3
- : unit = ()

#  cycle sim_verilator 23 34;
23 + 34 = 57
- : unit = ()
```

Notice the type signature of `sim_verilator` being a simple
`Cyclesim.t_port_list`. This means that we can write simulation test
benches that are agnostic to the backend, whether it is Hardcaml's
Cyclesim, Hardcaml Verilator, or Hardcaml C.

`Hardcaml_verilator` also supports an Interface-based API. See
[`Hardcaml_verilator.With_interface`](https://ocaml.org/p/hardcaml_verilator/latest/doc/Hardcaml_verilator/With_interface/index.html).

## Compilation options

Some verilator configuration options are exposed via [`Hardcaml_verilator.Config`]. In
particular, it is possible to split the generated C++ code into much smaller chunks and
use many more gcc processes which can drastically improve compilation time. See the config
module for some presets.

The code supports both version 4 and 5 of verilator - set the config value
[verilator_version] appropriately.

# `Hardcaml_c`

Hardcaml_c is a Hardcaml simulator that converts the design to a
C-based simulation model for improved performance.

The primary benefit of using Hardcaml_c is that it has a much more
modest compilation time (compared to `Hardcaml_verilator`), but is still
much more performant than the regular Hardcaml Cyclesim simulator.

To use `Hardcaml_c`, `gcc` has to be installed on your local machine.

The key limitation in Hardcaml C is that, as of writing, it does not
properly support tracing outputs before clock edges.

Using the `Hardcaml_c` simulation backend is simple, as demonstrated
in the example below.

```ocaml
# let sim_c =
    Hardcaml_c.Cyclesim.create circuit
  ;;
val sim_c : Cyclesim.t_port_list = <abstr>

# cycle sim_c 1 2;
1 + 2 = 3
- : unit = ()

# cycle sim_c 23 50;
23 + 50 = 73
- : unit = ()
```

Like Hardcaml Verilator, Hardcaml C supports an Interface-based
API. See [`Hardcaml_c.Cyclesim.With_interface`](https://ocaml.org/p/hardcaml_c/latest/doc/Hardcaml_c/Cyclesim/With_interface/index.html).
