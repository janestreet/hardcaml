library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity single_port_ram is
  port (
    clock, write_enable : in std_logic;
    address : in std_logic_vector(2 downto 0);
    write_data : in std_logic_vector(7 downto 0);
    read_data : out std_logic_vector(7 downto 0)
  );
end entity;

architecture rtl of single_port_ram is 

  type mem_t is array (0 to 7) of std_logic_vector(7 downto 0);
  signal mem : mem_t;
  signal read_data_int : std_logic_vector(7 downto 0);

begin

  read_data <= read_data_int;

  process (clock) is
  begin
      if rising_edge(clock) then
        if write_enable = '1' then
          mem(to_integer(unsigned(address))) <= write_data;
        end if;
        read_data_int <= mem(to_integer(unsigned(address)));
      end if;
  end process;

end architecture;
