-- We now require VHDL-2008 which has a whole load of new operators and
-- functions which might make the vhdl generator simpler. Lets test them out.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test_slv is
  port (
    pi1 : in std_logic;
    pi2 : in std_logic_vector(1 downto 0);
    po1 : out std_logic;
    po2 : out std_logic_vector(1 downto 0)
  );
end entity;

architecture test_slv of test_slv is

  constant v1 : std_logic_vector(1 downto 0) := "01";
  constant v2 : std_logic_vector(1 downto 0) := "10";
  constant b0 : std_logic := '0';
  constant b1 : std_logic := '1';

  signal vadd : std_logic_vector(1 downto 0);
  signal badd : std_logic;
  signal vsub : std_logic_vector(1 downto 0);
  signal bsub : std_logic;

  signal vconcat : std_logic_vector(5 downto 0);

  signal vselect : std_logic_vector(2 downto 0);
  signal bselect : std_logic;

  signal vlt : std_logic;
  signal blt, blt2 : std_logic;

  signal veq : std_logic;
  signal beq : std_logic;

  signal vnot : std_logic_vector(1 downto 0);
  signal bnot : std_logic;

  signal vand : std_logic_vector(1 downto 0);
  signal band : std_logic;
  signal vor : std_logic_vector(1 downto 0);
  signal bor : std_logic;
  signal vxor : std_logic_vector(1 downto 0);
  signal bxor : std_logic;

  signal vmul1 : std_logic_vector(2 downto 0); -- 2x1
  signal vmul2 : std_logic_vector(3 downto 0); -- 2x2
  signal vmul3 : std_logic_vector(1 downto 0); -- 1x1
  signal vmul4 : std_logic_vector(2 downto 0); -- 2x1
  signal vmul5 : std_logic_vector(3 downto 0); -- 2x2
  signal vmul6 : std_logic_vector(1 downto 0); -- 1x1
begin

  -- addition
  vadd <= std_logic_vector(unsigned(v1) + unsigned(v2));
  --badd <= b0 xor b1;
  badd <= (unsigned'("" & b0) + unsigned'("" & b1)) ?= "1";

  -- subtraction
  vsub <= std_logic_vector(unsigned(v1) - unsigned(v2));
  bsub <= (unsigned'("" & b0) - unsigned'("" & b1)) ?= "1";

  -- concatentate a mixture of vectors and bits
  vconcat <= b0 & v1 & v2 & b0;

  -- selection
  vselect <= vconcat(3 downto 1);
  bselect <= vconcat(4);

  -- less than
  vlt <= unsigned(v1) ?< unsigned(v2);
  -- 0 0 - 0
  -- 0 1 - 1
  -- 1 0 - 0
  -- 1 1 - 0
  blt <= (not b0) and b1;
  blt2 <= unsigned'("" & b0) ?< unsigned'("" & b1);

  -- less than
  veq <= unsigned(v1) ?= unsigned(v2);
  beq <= unsigned'("" & b0) ?= unsigned'("" & b1);

  -- logical
  vnot <= not v1;
  bnot <= not b1;
  vand <= v1 and v2;
  band <= b1 and b0;
  vor <= v1 or v2;
  bor <= b1 or b0;
  vxor <= v1 xor v2;
  bxor <= b1 xor b0;

  -- multiplication.  Hmmm
  vmul1 <= std_logic_vector(unsigned(v1) * unsigned'("" & b0));
  vmul2 <= std_logic_vector(unsigned(v1) * unsigned(v2));
  vmul3 <= std_logic_vector(unsigned'("" & b1) * unsigned'("" & b0));
  vmul4 <= std_logic_vector(signed'("" & b1) * signed(v1));
  vmul5 <= std_logic_vector(signed(v1) * signed(v2));
  vmul6 <= std_logic_vector(signed'("" & b1) * signed'("" & b0));

end architecture;
