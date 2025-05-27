library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity parity is
  port (
    d : in std_logic_vector(3 downto 0);
    q : out std_logic
  );
end entity;

architecture rtl of parity is
begin

  process (d) is 
    variable parity : std_logic;
  begin
    parity := '0';
    for i in d'range loop
      parity := parity xor d(i);
    end loop;
    q <= parity;
  end process;

end architecture;

