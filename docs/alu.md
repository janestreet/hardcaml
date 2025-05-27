# 2.1.5 ALU

# A Simple 8-bit ALU

A simple arithmetic logic unit. It takes two 8-bit operands `A` and `B` and computes an
8-bit output `q` according to the value of the operation select signal `op` as described in
the table below:

|op        | fn                        |
|----------|---------------------------|
|0         | A AND B                   |
|1         | A SUB B                   |
|2         | A MUL B                   |
|3         | A SHIFTLEFT 1             |
|4         | A SHIFTRIGHT 1            |
|5         | A AND B                   |
|6         | A OR B                    |
|7         | A XOR B                   |
|8         | NOT A                     |
|9         | 1 if A LESSTHAN B else 0  |
|10        | 1 if A EQUAL B else 0     |
|otherwise | 0                         |

## Verilog

<!-- $MDX file=./hdl/alu.v -->
```verilog
module alu (
  input [3:0] op,
  input [7:0] a, b,
  output reg [7:0] q
);

always @* begin
    case (op)
      0: q <= a + b;
      1: q <= a - b;
      2: q <= a * b;
      3: q <= a << 1;
      4: q <= a >> 1;
      5: q <= a & b;
      6: q <= a | b;
      7: q <= a ^ b;
      8: q <= ~a;
      9: q <= a < b;
      10: q <= a == b;
      default: q <= 0;
    endcase
  end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/alu.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu is
  port (
    op : in std_logic_vector(3 downto 0);
    a, b : in std_logic_vector(7 downto 0);
    q : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of alu is
  signal q_int : std_logic_vector(7 downto 0);
begin

  process (op, a, b) is
    variable mul : std_logic_vector(15 downto 0);
  begin
    case to_integer(unsigned(op)) is
      when 0 => q_int <= std_logic_vector(unsigned(a) + unsigned(b));
      when 1 => q_int <= std_logic_vector(unsigned(a) - unsigned(b));
      when 2 => 
        mul := std_logic_vector(unsigned(a) * unsigned(b));
        q_int <= mul(7 downto 0);
      when 3 => q_int <= std_logic_vector(shift_left(unsigned(a), 1));
      when 4 => q_int <= std_logic_vector(shift_right(unsigned(a), 1));
      when 5 => q_int <= a and b;
      when 6 => q_int <= a or b;
      when 7 => q_int <= a xor b;
      when 8 => q_int <= not a;
      when 9 => 
        if unsigned(a) < unsigned(b) then
          q_int <= "00000001";
        else
          q_int <= "00000000";
        end if;
      when 10 =>
        if a = b then
          q_int <= "00000001";
        else
          q_int <= "00000000";
        end if;
      when others => q_int <= (others => '0');
    end case;
  end process;

end architecture;
```

## Hardcaml

<!-- $MDX file=./lib/combinational_examples.ml,part=alu -->
```ocaml
  let alu ~op ~a ~b =
    mux
      op
      ([ a +: b
       ; a -: b
       ; a *: b
       ; sll a ~by:1
       ; srl a ~by:1
       ; a &: b
       ; a |: b
       ; a ^: b
       ; ~:a
       ; a <: b
       ; a ==: b
       ; zero 8
       ]
       |> List.map ~f:(uresize ~width:8))
  ;;
```

