library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ring_counter is
  generic (
    N : integer := 4
  );
  port (
    clock, clear : in std_logic;
    q : out std_logic_vector(N-1 downto 0)
  );
end entity;

architecture rtl of ring_counter is
    signal q_int : std_logic_vector(N-1 downto 0);
begin

  q <= q_int;

  process (clock) is
  begin
    if rising_edge(clock) then
      if clear = '1' then
        q_int <= std_logic_vector(to_unsigned(1, q_int'length));
      else
        q_int <= q_int(0) & q_int(N-1 downto 1);
      end if;
    end if;
  end process;

end architecture;
