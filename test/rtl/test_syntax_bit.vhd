-- We now require VHDL-2008 which has a whole load of new operators and
-- functions which might make the vhdl generator simpler. Lets test them out.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_bit.all;

entity test_bv is
end entity;

architecture test_bv of test_bv is

  function to_bit(s : std_ulogic) return bit is begin return to_bit(s, '0'); end;
  function to_bitvector(s : std_ulogic_vector) return bit_vector is begin return to_bitvector(s, '0'); end;

  constant v1 : bit_vector(1 downto 0) := "01";
  constant v2 : bit_vector(1 downto 0) := "10";
  constant b0 : bit := '0';
  constant b1 : bit := '1';

  signal vadd : bit_vector(1 downto 0);
  signal badd : bit;
  signal vsub : bit_vector(1 downto 0);
  signal bsub : bit;

  signal vconcat : bit_vector(5 downto 0);

  signal vselect : bit_vector(2 downto 0);
  signal bselect : bit;

  signal vlt : bit;
  signal blt, blt2 : bit;

  signal veq : bit;
  signal beq : bit;

  signal vnot : bit_vector(1 downto 0);
  signal bnot : bit;

  signal vand : bit_vector(1 downto 0);
  signal band : bit;
  signal vor : bit_vector(1 downto 0);
  signal bor : bit;
  signal vxor : bit_vector(1 downto 0);
  signal bxor : bit;

  signal vmul1 : bit_vector(2 downto 0); -- 2x1
  signal vmul2 : bit_vector(3 downto 0); -- 2x2
  signal vmul3 : bit_vector(1 downto 0); -- 1x1
  signal vmul4 : bit_vector(2 downto 0); -- 2x1
  signal vmul5 : bit_vector(3 downto 0); -- 2x2
  signal vmul6 : bit_vector(1 downto 0); -- 1x1


  signal pi1 : bit;
  signal pi2 : bit_vector(1 downto 0);
  signal po1 : bit;
  signal po2 : bit_vector(1 downto 0);
begin

  -- addition
  vadd <= bit_vector(unsigned(v1) + unsigned(v2));
  --badd <= b0 xor b1;
  badd <= (unsigned'("" & b0) + unsigned'("" & b1)) ?= "1";

  -- subtraction
  vsub <= bit_vector(unsigned(v1) - unsigned(v2));
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
  vmul1 <= bit_vector(unsigned(v1) * unsigned'("" & b0));
  vmul2 <= bit_vector(unsigned(v1) * unsigned(v2));
  vmul3 <= bit_vector(unsigned'("" & b1) * unsigned'("" & b0));
  vmul4 <= bit_vector(signed'("" & b1) * signed(v1));
  vmul5 <= bit_vector(signed(v1) * signed(v2));
  vmul6 <= bit_vector(signed'("" & b1) * signed'("" & b0));

  -- This shows how we can perform conversion from std_logic to bit types on
  -- the way in an out of an instantiation.  Its unfortunate that we need to
  -- redefine the to_bit and to_bitvector conversions due to the extra xmap
  -- default argument, but I cannot find a suitable replacement.
  the_slv: entity work.test_slv(test_slv)
    port map ( pi1 => to_stdulogic(pi1), pi2 => to_stdlogicvector(pi2), to_bit(po1) => po1, to_bitvector(po2) => po2 );

end architecture;
