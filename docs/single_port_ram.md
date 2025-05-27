# 2.2.9 Single Port RAM

# Single Port RAM

This is a synchronous single-port RAM with the following specifications:

- **Memory Size**: 8 words × 8 bits (64 bits total)
  - Address width: 3 bits (2^3^ = 8 addresses)
  - Data width: 8 bits (each memory location stores a byte)

- **Port Configuration**: Single-port design
  - One shared address bus for both reads and writes
  - One data input bus
  - One data output bus

- **Timing Behavior**: Synchronous operation
  - All operations occur on the rising edge of the clock
  - Write operations happen when `write_enable` is active
  - Read operations happen on every clock cycle regardless of `write_enable`
  - Read behavior during simultaneous write is consistent (reads the previous value, not the new one)

## Verilog

<!-- $MDX file=./hdl/single_port_ram.v -->
```verilog
module single_port_ram (
  input clock, write_enable,
  input [2:0] address,
  input [7:0] write_data,
  output reg [7:0] read_data
);

  reg [7:0] mem[0:7];

  always @(posedge clock) begin
    if (write_enable)
      mem[address] <= write_data;
    read_data <= mem[address];
  end

endmodule
```
## VHDL


<!-- $MDX file=./hdl/single_port_ram.vhd -->
```vhdl
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
```

## Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=single_port_ram -->
```ocaml
  let single_port_ram ~clock ~address ~write_enable ~write_data =
    let spec = Reg_spec.create ~clock () in
    (multiport_memory
       (Int.pow 2 (width address))
       ~write_ports:
         [| { write_clock = clock; write_enable; write_address = address; write_data } |]
       ~read_addresses:[| address |]).(0)
    |> reg spec
  ;;
```
