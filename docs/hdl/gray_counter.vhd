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

