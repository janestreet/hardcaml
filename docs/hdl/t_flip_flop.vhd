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

