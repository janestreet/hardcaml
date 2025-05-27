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


