# 2.1.3 Priority Encoder

# Priority Encoder 

## Verilog

<!-- $MDX file=./hdl/priority_encoder.v -->
```verilog
module priority_encoder (
  input [3:0] sel,
  input [7:0] a, b, c, d,
  output reg [7:0] q
);

  always @* begin
    if (sel[3]) q <= d;
    else if (sel[2]) q <= c;
    else if (sel[1]) q <= b;
    else q <= a;
  end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/priority_encoder.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;

entity priority_encoder is
  port (
    sel : in std_logic_vector(3 downto 0);
    a, b, c, d : in std_logic_vector(7 downto 0);
    q : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of priority_encoder is
begin

  q <= d when sel(3) = '1' else
       c when sel(2) = '1' else 
       b when sel(1) = '1' else
       a;

end architecture;
```

## Hardcaml

### Direct translation

<!-- $MDX file=./lib/combinational_examples.ml,part=priority_encoder_1 -->
```ocaml
  let priority_encoder_1 ~sel ~a ~b ~c ~d =
    let out = Always.Variable.wire ~default:(zero 8) () in
    Always.(
      compile
        [ if_ sel.:(3) [ out <-- d ]
          @@ elif sel.:(2) [ out <-- c ]
          @@ elif sel.:(1) [ out <-- b ]
          @@ [ out <-- a ]
        ]);
    out.value
  ;;
```

### Idiomatic Hardcaml

<!-- $MDX file=./lib/combinational_examples.ml,part=priority_encoder_2 -->
```ocaml
  let priority_encoder_2 ~sel ~a ~b ~c ~d =
    priority_select_with_default
      ~default:a
      With_valid.
        [ { valid = sel.:(3); value = d }
        ; { valid = sel.:(2); value = c }
        ; { valid = sel.:(1); value = b }
        ]
  ;;
```
