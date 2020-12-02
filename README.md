"Hardcaml"
==========

Hardcaml is an OCaml library for designing hardware.

* Express hardware designs in OCaml
* Make generic designs using higher order functions, lists, maps, functors...
* Simulate designs in OCaml
* Convert to VHDL, Verilog
* Write new modules to transform or analyse circuits, or provide new backends

# Install

```
$ opam install hardcaml ppx_deriving_hardcaml hardcaml_waveterm
```

# Documentation

* [Manual](https://github.com/janestreet/hardcaml/blob/master/docs/intro.mdx)
* [API Docs](https://ocaml.janestreet.com/ocaml-core/latest/doc/hardcaml)

# Related tools and libraries

* `Hardcaml_c` - convert Hardcaml designs to C-based simulation models
* `Hardcaml_circuits` - a library of useful/interesting Hardcaml designs
* `Hardcaml_fixed_point` - fixed point arithmetic with rounding and overflow control
* `Hardcaml_of_verilog` - read a verilog design into Hardcaml using Yosys
* `Hardcaml_step_testbench` - monadic testbench api
* `Hardcaml_verify` - verification tools for Hardcaml
* `Hardcaml_verilator` - convert Hardcaml designs to very high speed simulation models
* `Hardcaml_waveterm` - ASCII based digital waveforms
* `Hardcaml_xilinx` - various Xilinx primitives wrapped with Hardcaml interfaces
   and simulation models
* `Hardcaml_xilinx_components` - tool to read Xilinx unisim and xpm
  component definitions and generate Hardcaml interfaces
