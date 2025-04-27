# RTL Generation

<!--
```ocaml
# Hardcaml.Caller_id.set_mode Disabled
- : unit = ()
```
-->

You can [convert](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Rtl/index.html)
a Hardcaml [`Circuit`](https://ocaml.org/p/hardcaml/latest/doc/Hardcaml/Circuit/index.html)
to either Verilog or VHDL.

The following is a trivial example.

```ocaml
open Base
open Hardcaml
open Signal
let circuit = Circuit.create_exn ~name:"test" [ output "b" (input "a" 1) ]
```

The simplest function to use is `print` which outputs the circuit to
`stdout`. The `output` function provides other options for where to
write the code.

```ocaml
# let () = Rtl.print Verilog circuit
module test (
    a,
    b
);

    input a;
    output b;

    assign b = a;

endmodule
```

```ocaml
# let () = Rtl.print Vhdl circuit
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port (
        a : in std_logic;
        b : out std_logic
    );
end entity;

architecture rtl of test is


begin

    b <= a;

end architecture;
```

# Hierarchy

By default, instantiations are written to the generated RTL code, and
nothing further is done. This is useful when instantiating external IP
(such as Xilinx or Altera IPs) provided in separate RTL files.

If the optional `database` argument is provided, then instantiations
are looked up in a `Circuit_database.t`. This allows a
[module hierarchy](module_hierarchy.md) specified in Hardcaml to
be reflected in the generated Verilog or VHDL code. Only the top-level
module needs to be provided, and the RTL generator will traverse the
hierarchy as appropriate.

# Output modes

The optional `output_mode` argument controls how to output the design.
`To_file` will write it to a single file with the given name.
`To_buffer` and `To_channel` write to a `Buffer.t` and `Out_channel.t`,
respectively.

`To_directory` writes to the given directory. Filenames are based on
the corresponding circuit name. For hierarchical designs, multiple
files are generated.
