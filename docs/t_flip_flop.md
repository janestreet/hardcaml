# 2.2.2 T Flip Flop

# T Flip Flop

The T (Toggle) flip-flop changes state based on a single control input `t`:

- When t=0: Maintains current output state
- When t=1: Toggles (inverts) the current output state

The flip-flop updates only on the rising edge of the clock signal. This implementation
includes an active low asynchronous reset, which means:

- When reset=0: Output is forced to 0 immediately, regardless of clock
- When reset=1: Normal operation based on `t` input

## Verilog

<!-- $MDX file=./hdl/t_flip_flop.v -->
```verilog
module t_flip_flop (
  input clock, reset_n, t,
  output reg q
);

  always @(posedge clock, negedge reset_n) begin
    if (!reset_n)
      q <= 1'b0;
    else if (t)
      q <= ~q;
  end

endmodule
```

## VHDL 

<!-- $MDX file=./hdl/t_flip_flop.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity t_flip_flop is
  port (
    clock, reset_n, t : in std_logic;
    q : out std_logic
  );
end entity;

architecture rtl of t_flip_flop is 
  signal q_int : std_logic;
begin

  q <= q_int;
  
  process (clock, reset_n) begin
    if reset_n = '0' then 
      q_int <= '0';
    elsif rising_edge(clock) then
      if t = '1' then
        q_int <= not q_int;
      end if;
    end if;
  end process;

end architecture;
```

## Hardcaml

### Direct translation

<!-- $MDX file=./lib/sequential_examples.ml,part=t_flip_flop_1 -->
```ocaml
  let t_flip_flop_1 ~clock ~reset_n ~t =
    let q =
      Always.Variable.reg
        (Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Falling ())
        ~width:1
    in
    Always.(compile [ if_ t [ q <-- ~:(q.value) ] [ q <-- q.value ] ]);
    q.value
  ;;
```

### Idiomatic Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=t_flip_flop_2 -->
```ocaml
  let t_flip_flop_2 ~clock ~reset_n ~t =
    reg_fb
      (Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Falling ())
      ~width:1
      ~enable:t
      ~f:( ~: )
  ;;
```
