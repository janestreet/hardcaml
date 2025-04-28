library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity foo is
  port (
    a : in std_logic;
    b : out std_logic
    );
end entity;

architecture rtl of foo is 
begin

  b <= not a;

end architecture;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity bar is
  port (
    a : in std_logic;
    b : out std_logic
    );
  attribute keep_hierarchy : string;
end entity;

architecture rtl of bar is
  -- The purpose of this design is to show attributes being correctly assigned
  -- to an [entity work.foo(rtl)] style component instantiation. Run this in
  -- vivado and switch between TRUE and FALSE to see very different synthesis
  -- results from this circuit according to the attribute.
  attribute keep_hierarchy of the_foo : label is "FALSE";
  signal c : std_logic;
begin

  the_foo: entity work.foo(rtl) 
    port map (a => a, b => c);

  b <= not c;

end architecture;

