# 2.2.3 D Flip Flop

# D Flip Flop

The D (Data) flip-flop loads the input value to the output on the rising edge of the clock:

- When enable=1: Output `q` takes the value of input `d` on the clock edge
- When enable=0: Output `q` maintains its current value

This implementation includes two reset mechanisms:

- Active high asynchronous reset: Immediately sets `q` to 0 regardless of clock
- Synchronous clear: Sets `q` to 0 on the next clock edge when clear is high

The enable acts as a clock enable, allowing or preventing state changes on the clock edge.

## Verilog 

<!-- $MDX file=./hdl/d_flip_flop.v -->
```verilog
module d_flip_flop (
  input clock, clear, reset, enable, d,
  output reg q
);

  always @(posedge clock, posedge reset) begin
    if (reset)
      q <= 1'b1;
    else if (clear)
      q <= 1'b0;
    else if (enable)
      q <= d;
  end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/d_flip_flop.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity d_flip_flop is
  port (
    clock, reset, clear, enable, d : in std_logic;
    q : out std_logic
  );
end entity;

architecture rtl of d_flip_flop is 
  signal q_int : std_logic;
begin

  q <= q_int;
  
  process (clock, reset) begin
    if reset = '1' then
      q_int <= '1';
    elsif rising_edge(clock) then
      if clear = '1' then 
        q_int <= '0';
      elsif enable = '1' then
        q_int <= d;
      end if;
    end if;
  end process;

end architecture;
```

## Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=d_flip_flop -->
```ocaml
  let d_flip_flop ~clock ~reset ~clear ~enable ~d =
    reg (Reg_spec.create ~clock ~reset ~clear ()) ~enable d
  ;;
```

