# 2.1.4 Parity

# Parity

This module calculates odd parity for a 4-bit input. Given a 4-bit input `d`, it
outputs `1` if it contains an odd number of `1`s, and `0` otherwise.

Implemented by XORing all the bits together.


## Verilog

<!-- $MDX file=./hdl/parity.v -->
```verilog
module parity (
  input [3:0] d,
  output q
);

  assign q = ^d;

endmodule
```

## VHDL

<!-- $MDX file=./hdl/parity.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity parity is
  port (
    d : in std_logic_vector(3 downto 0);
    q : out std_logic
  );
end entity;

architecture rtl of parity is
begin

  process (d) is 
    variable parity : std_logic;
  begin
    parity := '0';
    for i in d'range loop
      parity := parity xor d(i);
    end loop;
    q <= parity;
  end process;

end architecture;
```

## Hardcaml


### Direct translation of Verilog

<!-- $MDX file=./lib/combinational_examples.ml,part=parity_1 -->
```ocaml
  let parity_1 ~d = reduce (bits_lsb d) ~f:( ^: )
```

### Direct translation of Vhdl

<!-- $MDX file=./lib/combinational_examples.ml,part=parity_2 -->
```ocaml
  let parity_2 ~d =
    let parity = ref gnd in
    for i = 0 to width d - 1 do
      parity := !parity ^: d.:(i)
    done;
    !parity
  ;;
```

### Idiomatic Hardcaml

<!-- $MDX file=./lib/combinational_examples.ml,part=parity_3 -->
```ocaml
  let parity_3 ~d = tree ~arity:2 (bits_lsb d) ~f:(reduce ~f:( ^: ))
```

When performing reductions in Hardcaml, we will often use the tree function. This
rearranges the computation from:

```
(((a ^ b) ^ c) ^ d)
```

into:

```
((a ^ b) ^ (c ^ d))
```

