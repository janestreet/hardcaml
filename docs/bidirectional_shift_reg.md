# 2.2.8 Bidirectional Shift Register

# Bidirectional Shift Register

A shift register that can move data in either direction based on a control signal.

- If `dir=0` shift the register left and add `d` at the least significant bit
- If `dir=1` shift the register right and add `d` at the most significant bit

## Verilog

<!-- $MDX file=./hdl/bidirectional_shift_reg.v -->
```verilog
module bidirectional_shift_reg #(
  parameter N = 4
) (
  input clock, clear, enable, dir, d,
  output reg [N-1:0] q
);

  always @(posedge clock) begin
    if (clear)
      q <= 0;
    else
      if (enable) 
        if (dir)
          q <= { d, q[N-1:1] };
        else
          q <= { q[N-2:0], d };
  end 

endmodule
```

## VHDL

<!-- $MDX file=./hdl/bidirectional_shift_reg.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity bidirectional_shift_reg is
  generic (
    N : integer := 4
  );
  port (
    clock, clear, enable, dir, d : in std_logic;
    q : out std_logic_vector(N-1 downto 0)
  );
end entity;

architecture rtl of bidirectional_shift_reg is
  signal q_int : std_logic_vector(N-1 downto 0);
begin

  q <= q_int;

  process (clock) is 
  begin
    if rising_edge(clock) then
      if clear = '1' then
        q_int <= (others => '0');
      else
        if enable = '1' then
          if dir = '1' then
            q_int <= d & q_int(N-1 downto 1);
          else
            q_int <= q_int(N-2 downto 0) & d;
          end if;
        end if;
      end if;
    end if;
  end process;

end architecture;
```

## Hardcaml

### Direct translation

<!-- $MDX file=./lib/sequential_examples.ml,part=bidirectional_shift_reg_1 -->
```ocaml
  let bidirectional_shift_reg_1 ~n ~clock ~clear ~enable ~dir ~d =
    let out = Always.Variable.reg (Reg_spec.create ~clock ~clear ()) ~width:n in
    Always.(
      compile
        [ when_
            enable
            [ if_
                dir
                [ out <-- d @: out.value.:[n - 1, 1] ]
                [ out <-- out.value.:[n - 2, 0] @: d ]
            ]
        ]);
    out.value
  ;;
```

### Idiomatic Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=bidirectional_shift_reg_2 -->
```ocaml
  let bidirectional_shift_reg_2 ~n ~clock ~clear ~enable ~dir ~d =
    reg_fb (Reg_spec.create ~clock ~clear ()) ~width:n ~enable ~f:(fun out ->
      mux2 dir (d @: msbs out) (lsbs out @: d))
  ;;
```
