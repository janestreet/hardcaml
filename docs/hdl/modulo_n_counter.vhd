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

