library ieee;
use ieee.std_logic_1164.all;

entity \naming%is_hard\ is
  port (
    -- In VHDL, these are different identifiers.
    \foo\ : in std_logic;
    foo : in std_logic;
    -- But this is not allowed due to case sensistivity
    --FOO : in std_logic
    \entity\ : in std_logic;
    \reg\ : in std_logic
  );
end entity;

architecture rtl of \naming%is_hard\ is
begin
end architecture;


library ieee;
use ieee.std_logic_1164.all;

entity test is
end entity;

architecture test of test is
    signal \foo\ : std_logic;
    signal foo : std_logic;
    signal \entity\ : std_logic;
    signal \reg\ : std_logic;
begin

    the_thing: entity work.\naming%is_hard\(rtl)
        port map (
            \foo\ => \foo\,
            foo => foo,
            \entity\ => \entity\,
            \reg\ => \reg\
        );

end architecture;
