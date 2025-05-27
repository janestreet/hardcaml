# 2.1.1 ROM

# Rom 

8 entry ROM.  Outputs the address times 10.

## Verilog

<!-- $MDX file=./hdl/rom.v -->
```verilog
module rom (
  input [2:0] address,
  output reg [6:0] q
);

  always @* begin
    case (address)
      3'd0: q <= 7'd0;
      3'd1: q <= 7'd10;
      3'd2: q <= 7'd20;
      3'd3: q <= 7'd30;
      3'd4: q <= 7'd40;
      3'd5: q <= 7'd50;
      3'd6: q <= 7'd60;
      default: q <= 7'd70;
    endcase
  end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/rom.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom is
  port (
    address : in std_logic_vector(2 downto 0);
    q : out std_logic_vector(6 downto 0)
  );
end entity;

architecture rtl of rom is
  type rom_t is array (0 to 7) of integer;
  constant rom : rom_t := (0,10,20,30,40,50,60,70);
begin

  q <= std_logic_vector(to_unsigned(rom(to_integer(unsigned(address))), q'length));

end architecture;
```

## Hardcaml

<!-- $MDX file=./lib/combinational_examples.ml,part=rom -->
```ocaml
  let rom ~address =
    mux address (List.init 8 ~f:(fun i -> of_unsigned_int ~width:7 (i * 10)))
  ;;
```
