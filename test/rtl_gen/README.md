# Rtl generation tests

Generate RTL using a number of different circuits using VHDL and Verilog.

# Coverage

Coverage has been tested and is >90% - its basically only error cases that are not
covered.

There is a bit of set up needed to run coverage - add the bisect ppx to hardcaml and
disable javascript compilation where needed.

# Compiler foibles.

## GHDL

* ghdl has a warning about hiding definitions. The argument [a] to the
  various [hc_XXX] conversion functions can hide a port which leads to
  a warning (which I think is non-sensicle). -Wno-hide does not work
  (ghdl issue tracker notes this)

* ghdl can only compile a complete design. If instantiations are
  missing it will print an error.

## iverilog

* iverilog can only compile a complete design. If instantiations are
  missing it will print an error.
