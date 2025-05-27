library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu is
  port (
    op : in std_logic_vector(3 downto 0);
    a, b : in std_logic_vector(7 downto 0);
    q : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of alu is
  signal q_int : std_logic_vector(7 downto 0);
begin

  process (op, a, b) is
    variable mul : std_logic_vector(15 downto 0);
  begin
    case to_integer(unsigned(op)) is
      when 0 => q_int <= std_logic_vector(unsigned(a) + unsigned(b));
      when 1 => q_int <= std_logic_vector(unsigned(a) - unsigned(b));
      when 2 => 
        mul := std_logic_vector(unsigned(a) * unsigned(b));
        q_int <= mul(7 downto 0);
      when 3 => q_int <= std_logic_vector(shift_left(unsigned(a), 1));
      when 4 => q_int <= std_logic_vector(shift_right(unsigned(a), 1));
      when 5 => q_int <= a and b;
      when 6 => q_int <= a or b;
      when 7 => q_int <= a xor b;
      when 8 => q_int <= not a;
      when 9 => 
        if unsigned(a) < unsigned(b) then
          q_int <= "00000001";
        else
          q_int <= "00000000";
        end if;
      when 10 =>
        if a = b then
          q_int <= "00000001";
        else
          q_int <= "00000000";
        end if;
      when others => q_int <= (others => '0');
    end case;
  end process;

end architecture;
