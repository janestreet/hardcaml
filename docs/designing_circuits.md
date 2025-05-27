# 2. Designing Circuits

In this section we will learn the basics of working with hardware designs in Hardcaml.

We will start by looking at how to describe combinational logic and the functions that
Hardcaml provides for this purpose.

We will then look at sequential logic, where we introduce the clock signal, registers,
and memories.

Finally, we will look at the `Circuit.t` type which represents a module within a design. We
will also explore how to convert that to RTL (i.e., Verilog or VHDL).

A number of examples of combinational and sequential logic design are provided. In many
cases, we will show examples in Verilog, VHDL, and Hardcaml for comparison.
