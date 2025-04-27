library ieee;
use ieee.std_logic_1164.all;

entity inner is
  port (a : in std_logic; b : out std_logic);
  attribute foo : string;
  attribute bar : boolean;
  -- attributes of input/output ports
  attribute foo of a : signal is "hello";
  attribute bar of b : signal is false;
end entity;

architecture rtl of inner is

  signal q : std_logic;

  -- 2 attributes on a signal
  -- Note you cannot redefine the attribute type here if it was already done in
  -- the entity declaration.  Thus, we'll just stick all attributes decls in
  -- the entity.
  attribute foo of q : signal is "goodbye";
  attribute bar of q : signal is true;

begin

  q <= not a;
  b <= q;


end architecture;


library ieee;
use ieee.std_logic_1164.all;

entity outer is
  port (a : in std_logic; b : out std_logic);
  attribute tag : string;
end entity;

architecture rtl of outer is

  -- attributes on instantiations - the only way to apply this to an entity
  -- instantiation is via the label.
  attribute tag of the_inner : label is "bar";

begin

  the_inner: entity work.inner(rtl)
    port map (a => a, b => b);

end architecture;
