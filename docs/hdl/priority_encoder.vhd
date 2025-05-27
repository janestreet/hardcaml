library ieee;
use ieee.std_logic_1164.all;

entity priority_encoder is
  port (
    sel : in std_logic_vector(3 downto 0);
    a, b, c, d : in std_logic_vector(7 downto 0);
    q : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of priority_encoder is
begin

  q <= d when sel(3) = '1' else
       c when sel(2) = '1' else 
       b when sel(1) = '1' else
       a;

end architecture;
