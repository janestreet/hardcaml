# 2.1.2 Mux4

# Mux 4 to 1

4 input multiplexer.

## Verilog

<!-- $MDX file=./hdl/mux4.v -->
```verilog
module mux4 (
  input [1:0] address,
  input [7:0] a, b, c, d,
  output reg [7:0] q
);
   always @* begin
      case (address)
        3'd0: q <= a;
        3'd1: q <= b;
        3'd2: q <= c;
        default: q <= d;
      endcase
   end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/mux4.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mux4 is
  port (
    address : in std_logic_vector(1 downto 0);
    a, b, c, d : in std_logic_vector(7 downto 0);
    q : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of mux4 is
  signal q_int : std_logic_vector(7 downto 0);
begin

  q <= q_int;

  process (address, a, b, c, d) is 
  begin
    case to_integer(unsigned(address)) is
      when 0 => q_int <= a;
      when 1 => q_int <= b;
      when 2 => q_int <= c;
      when others => q_int <= d;
    end case;
  end process;

end architecture;
```

## Hardcaml

<!-- $MDX file=./lib/combinational_examples.ml,part=mux4 -->
```ocaml
  let mux4 ~address ~a ~b ~c ~d = mux address [ a; b; c; d ]
```
