open! Base
open Hardcaml

let clock = Signal.input "clock" 1
let write_data = Signal.input "d" 32
let write_address = Signal.input "wa" 7
let write_enable = Signal.input "we" 1
let read_address = Signal.input "ra" 7

let circuit =
  let open Signal in
  Circuit.create_exn
    ~name:"mem"
    [ output
        "q1"
        (memory
           128
           ~write_port:{ write_clock = clock; write_address; write_enable; write_data }
           ~read_address)
    ; output
        "q2"
        (memory
           16
           ~write_port:
             { write_clock = clock
             ; write_address = write_address.:[3, 0]
             ; write_enable
             ; write_data
             }
           ~read_address:read_address.:[3, 0]
         -- "q1"
         -- "q2")
    ; output
        "q3"
        (memory
           2
           ~write_port:
             { write_clock = clock
             ; write_address = write_address.:[0, 0]
             ; write_enable
             ; write_data = write_data.:[0, 0]
             }
           ~read_address:read_address.:[0, 0]
         -- "q1"
         -- "q2")
    ]
;;

let%expect_test "memory " =
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module mem (
        we,
        d,
        wa,
        clock,
        ra,
        q1,
        q2,
        q3
    );

        input we;
        input [31:0] d;
        input [6:0] wa;
        input clock;
        input [6:0] ra;
        output [31:0] q1;
        output [31:0] q2;
        output q3;

        wire signal_select;
        wire signal_select_1;
        reg [0:0] signal_multiport_mem[0:1];
        wire signal_select_2;
        wire q2_0;
        wire q1_0;
        wire [3:0] signal_select_3;
        reg [31:0] signal_multiport_mem_1[0:15];
        wire [3:0] signal_select_4;
        wire [31:0] q2_1;
        wire [31:0] q1_1;
        reg [31:0] signal_multiport_mem_2[0:127];
        wire [31:0] signal_mem_read_port;
        assign signal_select = d[0:0];
        assign signal_select_1 = wa[0:0];
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem[signal_select_1] <= signal_select;
        end
        assign signal_select_2 = ra[0:0];
        assign q2_0 = signal_multiport_mem[signal_select_2];
        assign signal_select_3 = wa[3:0];
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem_1[signal_select_3] <= d;
        end
        assign signal_select_4 = ra[3:0];
        assign q2_1 = signal_multiport_mem_1[signal_select_4];
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem_2[wa] <= d;
        end
        assign signal_mem_read_port = signal_multiport_mem_2[ra];
        assign q1_0 = q2_0;
        assign q1_1 = q2_1;
        assign q1 = signal_mem_read_port;
        assign q2 = q2_1;
        assign q3 = q2_0;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity mem is
        port (
            we : in std_logic;
            d : in std_logic_vector(31 downto 0);
            wa : in std_logic_vector(6 downto 0);
            clock : in std_logic;
            ra : in std_logic_vector(6 downto 0);
            q1 : out std_logic_vector(31 downto 0);
            q2 : out std_logic_vector(31 downto 0);
            q3 : out std_logic
        );
    end entity;

    architecture rtl of mem is

        signal signal_select : std_logic;
        signal signal_select_1 : std_logic;
        type signal_multiport_mem_type is array (0 to 1) of std_logic;
        signal signal_multiport_mem : signal_multiport_mem_type;
        signal signal_select_2 : std_logic;
        signal q2_0 : std_logic;
        signal q1_0 : std_logic;
        signal signal_select_3 : std_logic_vector(3 downto 0);
        type signal_multiport_mem_type_1 is array (0 to 15) of std_logic_vector(31 downto 0);
        signal signal_multiport_mem_1 : signal_multiport_mem_type_1;
        signal signal_select_4 : std_logic_vector(3 downto 0);
        signal q2_1 : std_logic_vector(31 downto 0);
        signal q1_1 : std_logic_vector(31 downto 0);
        type signal_multiport_mem_type_2 is array (0 to 127) of std_logic_vector(31 downto 0);
        signal signal_multiport_mem_2 : signal_multiport_mem_type_2;
        signal signal_mem_read_port : std_logic_vector(31 downto 0);

    begin

        signal_select <= d(0);
        signal_select_1 <= wa(0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem(to_integer(unsigned(std_logic_vector'("" & signal_select_1)))) <= signal_select;
                end if;
            end if;
        end process;
        signal_select_2 <= ra(0);
        q2_0 <= signal_multiport_mem(to_integer(unsigned(std_logic_vector'("" & signal_select_2))));
        signal_select_3 <= wa(3 downto 0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem_1(to_integer(unsigned(signal_select_3))) <= d;
                end if;
            end if;
        end process;
        signal_select_4 <= ra(3 downto 0);
        q2_1 <= signal_multiport_mem_1(to_integer(unsigned(signal_select_4)));
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem_2(to_integer(unsigned(wa))) <= d;
                end if;
            end if;
        end process;
        signal_mem_read_port <= signal_multiport_mem_2(to_integer(unsigned(ra)));
        q1_0 <= q2_0;
        q1_1 <= q2_1;
        q1 <= signal_mem_read_port;
        q2 <= q2_1;
        q3 <= q2_0;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog
    ~config:{ Rtl.Config.default with two_state = true }
    ~show:true
    circuit;
  [%expect
    {|
    module mem (
        we,
        d,
        wa,
        clock,
        ra,
        q1,
        q2,
        q3
    );

        input we;
        input [31:0] d;
        input [6:0] wa;
        input clock;
        input [6:0] ra;
        output [31:0] q1;
        output [31:0] q2;
        output q3;

        bit signal_select;
        bit signal_select_1;
        reg [0:0] signal_multiport_mem[0:1];
        bit signal_select_2;
        bit q2_0;
        bit q1_0;
        bit [3:0] signal_select_3;
        reg [31:0] signal_multiport_mem_1[0:15];
        bit [3:0] signal_select_4;
        bit [31:0] q2_1;
        bit [31:0] q1_1;
        reg [31:0] signal_multiport_mem_2[0:127];
        bit [31:0] signal_mem_read_port;
        assign signal_select = d[0:0];
        assign signal_select_1 = wa[0:0];
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem[signal_select_1] <= signal_select;
        end
        assign signal_select_2 = ra[0:0];
        assign q2_0 = signal_multiport_mem[signal_select_2];
        assign signal_select_3 = wa[3:0];
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem_1[signal_select_3] <= d;
        end
        assign signal_select_4 = ra[3:0];
        assign q2_1 = signal_multiport_mem_1[signal_select_4];
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem_2[wa] <= d;
        end
        assign signal_mem_read_port = signal_multiport_mem_2[ra];
        assign q1_0 = q2_0;
        assign q1_1 = q2_1;
        assign q1 = signal_mem_read_port;
        assign q2 = q2_1;
        assign q3 = q2_0;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_bit.all;

    entity mem is
        port (
            we : in bit;
            d : in bit_vector(31 downto 0);
            wa : in bit_vector(6 downto 0);
            clock : in bit;
            ra : in bit_vector(6 downto 0);
            q1 : out bit_vector(31 downto 0);
            q2 : out bit_vector(31 downto 0);
            q3 : out bit
        );
    end entity;

    architecture rtl of mem is
        -- Conversions
        function to_stdlogic(i : in bit) return std_logic is
        begin
            if i = '0' then
                return '0';
            else
                return '1';
            end if;
        end function;

        signal signal_select : bit;
        signal signal_select_1 : bit;
        type signal_multiport_mem_type is array (0 to 1) of std_logic;
        signal signal_multiport_mem : signal_multiport_mem_type;
        signal signal_select_2 : bit;
        signal q2_0 : bit;
        signal q1_0 : bit;
        signal signal_select_3 : bit_vector(3 downto 0);
        type signal_multiport_mem_type_1 is array (0 to 15) of std_logic_vector(31 downto 0);
        signal signal_multiport_mem_1 : signal_multiport_mem_type_1;
        signal signal_select_4 : bit_vector(3 downto 0);
        signal q2_1 : bit_vector(31 downto 0);
        signal q1_1 : bit_vector(31 downto 0);
        type signal_multiport_mem_type_2 is array (0 to 127) of std_logic_vector(31 downto 0);
        signal signal_multiport_mem_2 : signal_multiport_mem_type_2;
        signal signal_mem_read_port : bit_vector(31 downto 0);

    begin

        signal_select <= d(0);
        signal_select_1 <= wa(0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem(to_integer(unsigned'("" & signal_select_1))) <= to_stdlogic(signal_select);
                end if;
            end if;
        end process;
        signal_select_2 <= ra(0);
        q2_0 <= to_bit(signal_multiport_mem(to_integer(unsigned'("" & signal_select_2))));
        signal_select_3 <= wa(3 downto 0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem_1(to_integer(unsigned(signal_select_3))) <= to_stdlogicvector(d);
                end if;
            end if;
        end process;
        signal_select_4 <= ra(3 downto 0);
        q2_1 <= to_bitvector(signal_multiport_mem_1(to_integer(unsigned(signal_select_4))));
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem_2(to_integer(unsigned(wa))) <= to_stdlogicvector(d);
                end if;
            end if;
        end process;
        signal_mem_read_port <= to_bitvector(signal_multiport_mem_2(to_integer(unsigned(ra))));
        q1_0 <= q2_0;
        q1_1 <= q2_1;
        q1 <= signal_mem_read_port;
        q2 <= q2_1;
        q3 <= q2_0;

    end architecture;
    |}]
;;
