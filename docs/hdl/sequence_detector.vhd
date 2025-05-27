library ieee;
use ieee.std_logic_1164.all;

entity sequence_detector_vhdl is
  port (
    clock, clear, d : in std_logic;
    detect : out std_logic
  );
end entity;

architecture rtl of sequence_detector_vhdl is

  type state_t is (S1, S10, S101);
  signal state : state_t;

begin

  process (clock) begin
    if rising_edge(clock) then 
      if clear = '1' then 
        state <= S1;
        detect <= '0';
      else
        case (state) is
          when S1 => 
            if d = '1' then state <= S10;
            else state <= S1;
            end if;
          when S10 => 
            if d = '1' then state <= S1;
            else state <= S101;
            end if;
          when S101 => 
            state <= S1;
        end case;

        if state = S101 and d = '1' then
          detect <= '1';
        else
          detect <= '0';
        end if;
      end if;
    end if;
  end process;

end architecture;
