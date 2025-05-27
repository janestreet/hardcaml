library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom is
  port (
    address : in std_logic_vector(2 downto 0);
    q : out std_logic_vector(6 downto 0)
  );
end entity;

architecture rtl of rom is
  type rom_t is array (0 to 7) of integer;
  constant rom : rom_t := (0,10,20,30,40,50,60,70);
begin

  q <= std_logic_vector(to_unsigned(rom(to_integer(unsigned(address))), q'length));

end architecture;
