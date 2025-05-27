library ieee;
use ieee.std_logic_1164.all;

entity jk_flip_flop is
  port (
    clock, j, k : in std_logic;
    q : out std_logic
  );
end entity;

architecture rtl of jk_flip_flop is
  signal q_int : std_logic;
  signal jk : std_logic_vector(1 downto 0);
begin

  jk <= j & k;
  q <= q_int;

  process (clock) is
  begin
    if rising_edge(clock) then
      case jk is
        when "01" => q_int <= '0';
        when "10" => q_int <= '1';
        when "11" => q_int <= not q_int;
        when others => null;
      end case;
    end if;
  end process;


end architecture;
