library ieee;
use ieee.std_logic_1164.all;

entity bidirectional_shift_reg is
  generic (
    N : integer := 4
  );
  port (
    clock, clear, enable, dir, d : in std_logic;
    q : out std_logic_vector(N-1 downto 0)
  );
end entity;

architecture rtl of bidirectional_shift_reg is
  signal q_int : std_logic_vector(N-1 downto 0);
begin

  q <= q_int;

  process (clock) is 
  begin
    if rising_edge(clock) then
      if clear = '1' then
        q_int <= (others => '0');
      else
        if enable = '1' then
          if dir = '1' then
            q_int <= d & q_int(N-1 downto 1);
          else
            q_int <= q_int(N-2 downto 0) & d;
          end if;
        end if;
      end if;
    end if;
  end process;

end architecture;
