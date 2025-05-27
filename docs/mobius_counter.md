# 2.2.5 Mobius Counter

# Mobius Counter

Mobius counter is a really cool name for this, but it's probably more commonly known as a
Johnson counter. It is similar to a ring counter except the least significant bit is
inverted before being placed into the most significant bit.

This inversion creates a 2N-state counter from an N-bit register, as the counter cycles
through both normal and complemented patterns before repeating.

### Verilog 

<!-- $MDX file=./hdl/mobius_counter.v -->
```verilog
module mobius_counter #(
  parameter N = 4
) (
  input clock, clear,
  output reg [N-1:0] q
);

  always @(posedge clock) begin
    if (clear)
      q <= 0;
    else
      q <= { ~q[0], q[N-1:1] };
  end 

endmodule
```

### VHDL

<!-- $MDX file=./hdl/mobius_counter.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity mobius_counter is
  generic (
    N : integer := 4
  );
  port (
    clock, clear : in std_logic;
    q : out std_logic_vector(N-1 downto 0)
  );
end entity;

architecture rtl of mobius_counter is
    signal q_int : std_logic_vector(N-1 downto 0);
begin

  q <= q_int;

  process (clock) is
  begin
    if rising_edge(clock) then
      if clear = '1' then
        q_int <= (others => '0');
      else
        q_int <= (not q_int(0)) & q_int(N-1 downto 1);
      end if;
    end if;
  end process;

end architecture;
```

### Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=mobius_counter -->
```ocaml
  let mobius_counter ~n ~clock ~clear =
    reg_fb (Reg_spec.create ~clock ~clear ()) ~width:n ~f:(fun d -> ~:(lsb d) @: msbs d)
  ;;
```
