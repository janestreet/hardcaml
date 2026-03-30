"Hardcaml"
==========

<h1 align="center">
  <picture>
    <img src="docs/hardcaml.png">
  </picture>
  <br>
  Hardcaml
</h1>

Hardcaml is an OCaml library for designing and testing hardware designs. Hardcaml includes 
a simulation backend as well as support for compiling down to RTL, so an entire hardware 
project, from designing the hardware to validating the design to synthesizing it for an FPGA, 
can be done in OCaml. Hardcaml allows you to:

* Express hardware designs in OCaml
* Make generic designs using higher order functions, lists, maps, functors...
* Simulate designs in OCaml
* Convert to (hierarchical) Verilog or VHDL
* Write new modules to transform or analyse circuits, or provide new backends

To get the advantages of implementing software algorithms in hardware, you usually have to 
"unroll" iterative or sequential operations into a hardware pipeline that can compute many 
things in parallel. Traditional Hardware Description Languages offer only very primitive 
generation features, so this unrolling becomes unwieldy, and tends to result in code that's 
hard to read and to reuse across projects.

Hardcaml's strong type system and expressive metaprogramming capabilities allow you to implement 
highly flexible circuits concisely using OCaml. This also eliminates the risk of type-confusion 
bugs (in hardware, everything's just a wire, but we can assign semantic meanings to those wires 
using OCaml's type system), and makes the resultant code easier to understand and adapt. 
The ability to parametrize components using functors and higher-order functions lets you easily 
reuse them across designs and projects.

# Installation

```
$ opam install hardcaml ppx_hardcaml hardcaml_waveterm
```

# Testing hardware designs

Digital waveforms are commonly used during hardware development to capture the time-varying behaviour 
of signals relative to one another. Using the [`Hardcaml_waveterm`](https://github.com/janestreet/hardcaml_waveterm) 
library we can print waveforms from Hardcaml simulations.

```ocaml
let testbench () =
  let sim = Simulator.create create in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let step ~clear ~incr =
    inputs.clear := if clear=1 then Bits.vdd else Bits.gnd;
    inputs.incr := if incr=1 then Bits.vdd else Bits.gnd;
    Cyclesim.cycle sim
  in
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:1;
  step ~clear:0 ~incr:1;
  step ~clear:1 ~incr:0;
  step ~clear:0 ~incr:0;
  step ~clear:0 ~incr:0;
  waves
;;
val testbench : unit -> Waveform.t = <fun>

let waves = testbench ();;
val waves : Waveform.t = <abstr>
Waveform.print ~display_height:12 waves;;
┌Signals────────┐┌Waves──────────────────────────────────────────────┐
│clock          ││┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌──│
│               ││    └───┘   └───┘   └───┘   └───┘   └───┘   └───┘  │
│clear          ││                        ┌───────┐                  │
│               ││────────────────────────┘       └───────────────   │
│incr           ││        ┌───────────────┐                          │
│               ││────────┘               └───────────────────────   │
│               ││────────────────┬───────┬───────┬───────────────   │
│dout           ││ 00             │01     │02     │00                │
│               ││────────────────┴───────┴───────┴───────────────   │
│               ││                                                   │
└───────────────┘└───────────────────────────────────────────────────┘
- : unit = ()
```

# Documentation


* [Manual](https://github.com/janestreet/hardcaml/blob/master/docs/index.md)
* [API Docs](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/index.html)

# Projects using Hardcaml

* [Advent of Hardcaml](https://blog.janestreet.com/advent-of-hardcaml-2024/) - using FPGAs to solve Advent of Code problems.
* [`Hardcaml ZPrize`](https://zprize.hardcaml.com) -
  Multi-scalar Multiplication and Number Theoretic Transform accelerators.
* [`Hardcaml Mips`](https://github.com/askvortsov1/hardcaml-mips) - A
  simple 5-stage MIPs CPU with associated
  [blog post](https://ceramichacker.com/blog/1-1x-hardcaml-mips-intro-what-and-why)
  detailing the development process.
* [`Hardcaml_arty`](https://github.com/fyquah/hardcaml_arty) -
  Infrastructure targeting the [Arty
  A7](https://digilent.com/reference/programmable-logic/arty-a7/start)
  board.
* [`Hardcaml Reed-Solomon`](https://github.com/hardcamls/reedsolomon) -
  Configurable Reed-Solomon encoder and decoder implementation.
* [`Hardcaml JPEG`](https://github.com/hardcamls/video-coding/tree/main/jpeg) -
  JPEG decoder design.

# Tools and libraries

* [`Hardcaml_waveterm`](https://github.com/janestreet/hardcaml_waveterm) -
  ASCII based digital waveforms. Usable in expect tests or from an
  interactive terminal application.
* [`Hardcaml_c`](https://github.com/janestreet/hardcaml_c) - convert
  Hardcaml designs to C-based simulation models. Provides an API
  compatible with the standard Cyclesim module. Trades compilation
  time for runtime performance.
* [`Hardcaml_verilator`](https://github.com/janestreet/hardcaml_verilator) -
  Convert Hardcaml designs to very high speed simulation model using
  the open source [Verilator](https://www.veripool.org/verilator/) compiler.
* [`Hardcaml_step_testbench`](https://github.com/janestreet/hardcaml_step_testbench) -
  Monadic testbench API. Control multiple tasks synchronized to a
  clock without converting to a statemachine coding style.
* [`Hardcaml_circuits`](https://github.com/janestreet/hardcaml_circuits) -
  A library of useful/interesting Hardcaml designs
* [`Hardcaml_fixed_point`](https://github.com/janestreet/hardcaml_fixed_point) -
  Fixed point arithmetic with rounding and overflow control
* [`Hardcaml_xilinx`](https://github.com/janestreet/hardcaml_xilinx) -
   Various Xilinx primitives wrapped with Hardcaml interfaces and
   simulation models
* [`Hardcaml_xilinx_components`](https://github.com/janestreet/hardcaml_xilinx_components) -
  Tool to read Xilinx unisim and xpm component definitions and
  generate Hardcaml interfaces
* [`Hardcaml_of_verilog`](https://github.com/janestreet/hardcaml_of_verilog) -
  Convert a verilog design to Hardcaml using [Yosys](https://yosyshq.net/yosys/)
* [`Hardcaml_verify`](https://github.com/janestreet/hardcaml_verify) -
  SAT based formal verification tools for Hardcaml
* [`Hardcaml_xilinx_reports`](https://github.com/janestreet/hardcaml_xilinx_reports) -
  Automated generation of synthesis reports from Vivado.
