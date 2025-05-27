# 4. More on Circuit Design

We will now introduce some other parts of Hardcaml that are useful in generating realistic
hardware designs.

Naming will show how we can label nodes in Hardcaml designs for debugging.

You will have seen we used `Always` in some of the sequential logic examples. `Always`
models the behavioral style of writing hardware used in Verilog and VHDL. State machines
will also be introduced which use the `Always` style of coding.

`Instantiation`s allow us to link Hardcaml designs with external IP, as well as describe
hierarchical designs.

`Structural` will be introduced as a way of dealing with trisate values at the top level
of a hardware design.

