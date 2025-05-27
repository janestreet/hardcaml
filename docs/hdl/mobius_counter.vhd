library ieee;
use ieee.std_logic_1164.all;

entity mobius_counter is
  generic (
    N : integer := 4
  );
  port (
    clock, clear : in std_logic;
    q : out std_logic_vector(N-1 downto 0)
  );
end entity;

architecture rtl of mobius_counter is
    signal q_int : std_logic_vector(N-1 downto 0);
begin

  q <= q_int;

  process (clock) is
  begin
    if rising_edge(clock) then
      if clear = '1' then
        q_int <= (others => '0');
      else
        q_int <= (not q_int(0)) & q_int(N-1 downto 1);
      end if;
    end if;
  end process;

end architecture;

