# 2.2.4 Ring Counter

# Ring Counter

An N-bit right shift register that circularly shifts one bit through all positions. On
each clock cycle, the least significant bit shifts into the most significant bit.

Initialized with a single `1` bit (i.e., binary 00...01), which then circulates through
all bit positions creating a "one-hot" encoding pattern.

### Verilog 

<!-- $MDX file=./hdl/ring_counter.v -->
```verilog
module ring_counter #(
  parameter N = 4
) (
  input clock, clear,
  output reg [N-1:0] q
);

  always @(posedge clock) begin
    if (clear)
      q <= 1;
    else
      q <= { q[0], q[N-1:1] };
  end 

endmodule
```

### VHDL

<!-- $MDX file=./hdl/ring_counter.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ring_counter is
  generic (
    N : integer := 4
  );
  port (
    clock, clear : in std_logic;
    q : out std_logic_vector(N-1 downto 0)
  );
end entity;

architecture rtl of ring_counter is
    signal q_int : std_logic_vector(N-1 downto 0);
begin

  q <= q_int;

  process (clock) is
  begin
    if rising_edge(clock) then
      if clear = '1' then
        q_int <= std_logic_vector(to_unsigned(1, q_int'length));
      else
        q_int <= q_int(0) & q_int(N-1 downto 1);
      end if;
    end if;
  end process;

end architecture;
```

### Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=ring_counter -->
```ocaml
  let ring_counter ~n ~clock ~clear =
    reg_fb (Reg_spec.create ~clock ~clear ()) ~width:n ~clear_to:(one n) ~f:(rotr ~by:1)
  ;;
```
