# 2.2.7 Gray Counter

# Gray Counter

A Gray code is a binary number system in which each successive code differs by a single
bit. This property makes Gray codes useful for reducing switching noise, error detection,
and rotary encoders. Although sequential examples in this section focus on examples with a
single clock domain, Gray codes are also fundamental for safely transferring counters
across multiple clock domains.

Here we implement a Gray code counter. It is done by using a binary counter and converting
the value to a gray code.

## Binary to Gray code conversion

To convert a binary number to Gray code:

- The most significant bit remains unchanged
- Each remaining bit is XORed with the bit to its left

## Verilog

<!-- $MDX file=./hdl/gray_counter.v -->
```verilog
module gray_counter #(
  parameter N = 4
) (
  input clock, clear,
  output reg [N-1:0] q
);
  reg [N-1:0] count;

   always @(posedge clock) begin
     if (clear) begin
        count <= 0;
        q <= 0;
      end else begin
        count <= count + 1;
        q <= { count[N-1], count[N-1:1] ^ count[N-2:0] };
      end
   end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/gray_counter.vhd -->
```vhdl
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gray_counter is
  generic (
    N : integer := 4
  );
  port (
    clock, clear : in std_logic;
    q : out std_logic_vector(N-1 downto 0)
  );
end entity;

architecture rtl of gray_counter is 
  signal count : std_logic_vector(N-1 downto 0);
  signal q_int : std_logic_vector(N-1 downto 0);
begin

  q <= q_int;

  process (clock) begin
    if rising_edge(clock) then
      if clear = '1' then 
        count <= (others => '0');
        q_int <= (others => '0');
      else 
        count <= std_logic_vector(unsigned(count) + 1);
        q_int <= count(N-1) & (count(N-1 downto 1) xor count(N-2 downto 0));
      end if;
    end if;
  end process;

end architecture;
```

## Hardcaml

### Direct translation 

<!-- $MDX file=./lib/sequential_examples.ml,part=gray_counter_1 -->
```ocaml
  let gray_counter_1 ~n ~clock ~clear =
    let spec = Reg_spec.create ~clock ~clear () in
    let q = Always.Variable.reg spec ~width:n in
    let out = Always.Variable.reg spec ~width:n in
    Always.(
      compile
        [ q <-- q.value +:. 1
        ; out <-- q.value.:(n - 1) @: q.value.:[n - 1, 1] ^: q.value.:[n - 2, 0]
        ]);
    out.value
  ;;
```

### Idiomatic Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=gray_counter_2 -->
```ocaml
  let gray_counter_2 ~n ~clock ~clear =
    let spec = Reg_spec.create ~clock ~clear () in
    let q = reg_fb spec ~width:n ~f:(fun q -> q +:. 1) in
    reg spec (msb q @: msbs q ^: lsbs q)
  ;;
```
