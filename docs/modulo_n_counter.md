# 2.2.6 Modulo N Counter

# Modulo N Counter

On each clock cycle, this counter adds `1` to the current value if the `increment` signal
is high. If the current value equals `n-1`, it wraps around to `0`.

## Verilog

<!-- $MDX file=./hdl/modulo_n_counter.v -->
```verilog
module modulo_n_counter #(
  parameter width = 4,
  parameter n = 12
) (
  input clock, clear, increment,
  output reg [width-1:0] q
);

  always @(posedge clock)
  begin
    if (clear) 
      q <= 0;
    else if (increment)
      if (q == (n-1)) q <= 0;
      else q <= q + 1;
  end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/modulo_n_counter.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity modulo_n_counter is
  generic (
    width : integer := 4;
    n : integer := 12
  );
  port (
    clock, clear, increment : in std_logic;
    q : out std_logic_vector(width-1 downto 0)
  );
end entity;

architecture rtl of modulo_n_counter is
    signal q_int : std_logic_vector(width-1 downto 0);
begin

  q <= q_int;

  process (clock) is 
  begin
    if rising_edge(clock) then
      if clear = '1' then
        q_int <= (others => '0');
      elsif increment = '1' then
       if (q_int = std_logic_vector(to_unsigned(n-1, q'length))) then
          q_int <= (others => '0');
        else
          q_int <= std_logic_vector(unsigned(q_int) +  1);
        end if;
      end if;
    end if;
  end process;

end architecture;
```

## Hardcaml

### Direct translation

<!-- $MDX file=./lib/sequential_examples.ml,part=modulo_n_counter_1 -->
```ocaml
  let modulo_n_counter_1 ~config:{ width; n } ~clock ~clear ~increment =
    let spec = Reg_spec.create ~clock ~clear () in
    let out = Always.Variable.reg spec ~width in
    Always.(
      compile
        [ when_
            increment
            [ if_ (out.value ==:. n - 1) [ out <--. 0 ] [ out <-- out.value +:. 1 ] ]
        ]);
    out.value
  ;;
```

### Idiomatic Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=modulo_n_counter_2 -->
```ocaml
  let modulo_n_counter_2 ~config:{ width; n } ~clock ~clear ~increment =
    reg_fb
      (Reg_spec.create ~clock ~clear ())
      ~enable:increment
      ~width
      ~f:(mod_counter ~max:(n - 1))
  ;;
```


In this implementation the function `mod_counter`, provided by `Comb.S`, performs the
increment function. It is a little smarter than the other versions in that it will detect
if `2**width = n` and rely on natural binary overflow to wrap to `0`, eliding the
comparison.

