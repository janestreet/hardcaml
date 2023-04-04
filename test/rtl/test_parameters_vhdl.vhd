library IEEE;
use IEEE.std_logic_1164.all;
use STD.textio.all;
use IEEE.std_logic_textio.all;

entity test_parameters_vhdl is
  generic (
    an_int : integer := 0;
    a_bool : boolean := false;
    a_string : string := "hello";
    a_real : real := 0.0;
    a_bit : bit := '0';
    a_bit_vector : bit_vector(3 downto 0) := (others=>'0');
    a_std_logic : std_logic := 'U';
    a_std_ulogic : std_ulogic := 'U';
    a_std_logic_vector : std_logic_vector(3 downto 0) := (others=>'0');
    a_std_ulogic_vector : std_ulogic_vector(3 downto 0) := (others=>'0')
  );
  port (
    a : in std_logic;
    b : out std_logic_vector(1 downto 0)
  );
end;

architecture rtl of test_parameters_vhdl is
begin

  process
    variable l : line;
  begin
    write(l, string'("VHDL")); writeline(output, l);
    write(l, string'("an_int              ")); write(l, an_int); writeline(output, l);
    write(l, string'("a_bool              ")); write(l, a_bool); writeline(output, l);
    write(l, string'("a_string            ")); write(l, a_string); writeline(output, l);
    write(l, string'("a_real              ")); write(l, a_real); writeline(output, l);
    write(l, string'("a_bit               ")); write(l, a_bit); writeline(output, l);
    write(l, string'("a_bit_vector        ")); write(l, a_bit_vector); writeline(output, l);
    write(l, string'("a_std_logic         ")); write(l, a_std_logic); writeline(output, l);
    write(l, string'("a_std_ulogic        ")); write(l, a_std_ulogic); writeline(output, l);
    write(l, string'("a_std_logic_vector  ")); write(l, a_std_logic_vector); writeline(output, l);
    write(l, string'("a_std_ulogic_vector ")); write(l, a_std_ulogic_vector); writeline(output, l);
    wait;
  end process;

  b <= a & a;

end architecture;
