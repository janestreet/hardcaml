library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sync_fifo is
  port (
    clock, clear, write, read : in std_logic;
    data_in : in std_logic_vector(15 downto 0);
    data_out : out std_logic_vector(15 downto 0);
    full, empty : out std_logic
  );
end entity;

architecture rtl of sync_fifo is
  type mem_t is array (0 to 7) of std_logic_vector(15 downto 0);
  signal mem : mem_t;

  signal wptr, rptr : unsigned(2 downto 0);
  signal write_incr : std_logic;
  signal read_incr : std_logic;
  signal full_int : std_logic;
  signal empty_int : std_logic;
begin

  full_int <= '1' when (wptr + 1) = rptr else '0';
  empty_int <= '1' when wptr = rptr else '0';
  full <= full_int;
  empty <= empty_int;

  write_incr <= write and (not full_int);
  read_incr <= read and (not full_int);

  process (clock) begin
    if rising_edge(clock) then
      if clear = '1' then
        rptr <= (others => '0');
        wptr <= (others => '0');
      else
        if write_incr = '1' then 
          wptr <= wptr + 1;
        end if;
        if read_incr = '1' then 
          rptr <= rptr + 1;
        end if;
      end if;

      if write_incr = '1' then
        mem(to_integer(wptr)) <= data_in;
      end if;
      if read_incr = '1' then
        data_out <= mem(to_integer(rptr));
      end if;
    end if;
  end process;
end architecture;
