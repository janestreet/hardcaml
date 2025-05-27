# 2.2.10 Synchronous FIFO

# Synchronous FIFO

A synchronous FIFO (First-In First-Out) buffer provides temporary storage between circuits
that process data at different rates but share the same clock domain. It allows a producer
to write data when ready and a consumer to read it later, while maintaining the original
order of the data.

## Verilog

<!-- $MDX file=./hdl/sync_fifo.v -->
```verilog
module sync_fifo (
  input clock, clear, write, read,
  input [15:0] data_in,
  output reg [15:0] data_out,
  output full, empty
);

  reg [2:0] wptr, rptr;
  reg [15:0] mem[0:7];

  assign full = (wptr + 1) == rptr;
  assign empty = wptr == rptr;

  wire write_incr = write & !full;
  wire read_incr = read & !empty;

  always @(posedge clock) begin
    if (clear) begin
      rptr <= 0;
      wptr <= 0;
    end else begin
      if (write_incr) wptr <= wptr + 1;
      if (read_incr) rptr <= rptr + 1;
    end
    if (write_incr) mem[wptr] <= data_in;
    if (read_incr) data_out <= mem[rptr];
  end

endmodule
```

## VHDL

<!-- $MDX file=./hdl/sync_fifo.vhd -->
```
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
```

## Hardcaml

<!-- $MDX file=./lib/sequential_examples.ml,part=sync_fifo -->
```ocaml
  type t =
    { data_out : Signal.t
    ; full : Signal.t
    ; empty : Signal.t
    }

  let sync_fifo ~clock ~clear ~write ~read ~data_in =
    let spec = Reg_spec.create ~clock ~clear () in
    let wptr = wire log_depth in
    let wptr_next = wptr +:. 1 in
    let rptr = wire log_depth in
    let full = wptr_next ==: rptr in
    let empty = wptr ==: rptr in
    let write_enable = write &: ~:full in
    let read_enable = read &: ~:empty in
    let mem =
      Ram.create
        ~collision_mode:Write_before_read
        ~size:depth
        ~write_ports:
          [| { write_clock = clock
             ; write_data = data_in
             ; write_enable
             ; write_address = wptr
             }
          |]
        ~read_ports:[| { read_clock = clock; read_enable; read_address = rptr } |]
        ()
    in
    wptr <-- reg spec ~enable:write_enable wptr_next;
    rptr <-- reg spec ~enable:read_enable (rptr +:. 1);
    { data_out = mem.(0); full; empty }
  ;;
```
