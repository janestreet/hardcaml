# 2.2.1 JK Flip Flop

# JK Flip Flop

The JK flip-flop is a basic sequential element with two inputs (`j` and `k`) that control its
behavior:

- Hold Mode: Maintains the current output value when j=0, k=0
- Set Mode: Sets the output value to 1 when j=1, k=0
- Reset Mode: Sets the output value to 0 when j=0, k=1
- Toggle Mode: Inverts the current output value when j=1, k=1

The flip-flop updates its state on the rising edge of the clock.

## Verilog

<!-- $MDX file=./hdl/jk_flip_flop.v -->
```verilog
module jk_flip_flop (
  input clock, j, k,
  output reg q
);

  always @(posedge clock) begin
    case ({j,k})
      2'b00: q <= q;
      2'b01: q <= 1'b0;
      2'b10: q <= 1'b1;
      2'b11: q <= ~q;
    endcase
  end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/jk_flip_flop.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity jk_flip_flop is
  port (
    clock, j, k : in std_logic;
    q : out std_logic
  );
end entity;

architecture rtl of jk_flip_flop is
  signal q_int : std_logic;
  signal jk : std_logic_vector(1 downto 0);
begin

  jk <= j & k;
  q <= q_int;

  process (clock) is
  begin
    if rising_edge(clock) then
      case jk is
        when "01" => q_int <= '0';
        when "10" => q_int <= '1';
        when "11" => q_int <= not q_int;
        when others => null;
      end case;
    end if;
  end process;


end architecture;
```

## Hardcaml

### Direct translation

<!-- $MDX file=./lib/sequential_examples.ml,part=jk_flip_flop_1 -->
```ocaml
  let jk_flip_flop_1 ~clock ~j ~k =
    let q = Always.Variable.reg (Reg_spec.create ~clock ()) ~width:1 in
    Always.(
      compile
        [ switch
            (j @: k)
            [ of_string "00", [ q <-- q.value ]
            ; of_string "01", [ q <-- gnd ]
            ; of_string "10", [ q <-- vdd ]
            ; of_string "11", [ q <-- ~:(q.value) ]
            ]
        ]);
    q.value
  ;;
```

### Idiomatic Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=jk_flip_flop_2 -->
```ocaml
  let jk_flip_flop_2 ~clock ~j ~k =
    reg_fb (Reg_spec.create ~clock ()) ~width:1 ~f:(fun q ->
      mux (j @: k) [ q; gnd; vdd; ~:q ])
  ;;
```
