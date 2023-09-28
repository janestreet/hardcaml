"Hardcaml"
==========

![](docs/hardcaml.png)

Hardcaml is an OCaml library for designing and testing hardware designs.

* Express hardware designs in OCaml
* Make generic designs using higher order functions, lists, maps, functors...
* Simulate designs in OCaml
* Convert to (hierarchical) Verilog or VHDL
* Write new modules to transform or analyse circuits, or provide new backends

# Install

```
$ opam install hardcaml ppx_hardcaml hardcaml_waveterm
```

# Documentation

* [Manual](https://github.com/janestreet/hardcaml/blob/master/docs/index.md)
* [API Docs](https://v3.ocaml.org/p/hardcaml/v0.15.0/doc/Hardcaml/index.html)

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

# Projects using Hardcaml

* [`Hardcaml ZPrize`](https://zprize.hardcaml.com) -
  Multi-scalar Multiplication and Number Theoretic Transform accelerators.
* [`Hardcaml Mips`](https://github.com/askvortsov1/hardcaml-mips) - A
  simple 5-stage MIPs CPU with associated
  [blog](https://ceramichacker.com/blog/1-1x-hardcaml-mips-intro-what-and-why)
  detailing the development process.
* [`Hardcaml_arty`](https://github.com/fyquah/hardcaml_arty) -
  Infrastructure targeting the [Arty
  A7](https://digilent.com/reference/programmable-logic/arty-a7/start)
  board.
* [`Hardcaml Reed-Solomon`](https://github.com/hardcamls/reedsolomon) -
  Configurable Reed-Solomon encoder and decoder implementation.
* [`Hardcaml JPEG`](https://github.com/hardcamls/video-coding/tree/main/jpeg) -
  JPEG decoder design.
