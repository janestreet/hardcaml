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

        wire _11;
        wire _10;
        reg [0:0] _12[0:1];
        wire _9;
        wire q2_0;
        wire q1_0;
        wire [3:0] _15;
        reg [31:0] _16[0:15];
        wire [3:0] _14;
        wire [31:0] q2_1;
        wire [31:0] q1_1;
        reg [31:0] _18[0:127];
        wire [31:0] _19;
        assign _11 = d[0:0];
        assign _10 = wa[0:0];
        always @(posedge clock) begin
            if (we)
                _12[_10] <= _11;
        end
        assign _9 = ra[0:0];
        assign q2_0 = _12[_9];
        assign _15 = wa[3:0];
        always @(posedge clock) begin
            if (we)
                _16[_15] <= d;
        end
        assign _14 = ra[3:0];
        assign q2_1 = _16[_14];
        always @(posedge clock) begin
            if (we)
                _18[wa] <= d;
        end
        assign _19 = _18[ra];
        assign q1_0 = q2_0;
        assign q1_1 = q2_1;
        assign q1 = _19;
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

        -- conversion functions
        function hc_uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;
        function hc_uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;
        function hc_sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;
        function hc_sgn(a : std_logic_vector) return signed           is begin return signed(a); end;
        function hc_sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;
        function hc_sl (a : unsigned)         return std_logic        is begin return a(a'right); end;
        function hc_sl (a : signed)           return std_logic        is begin return a(a'right); end;
        function hc_sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;
        function hc_slv(a : std_logic_vector) return std_logic_vector is begin return a; end;
        function hc_slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;
        function hc_slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;
        signal hc_11 : std_logic;
        signal hc_10 : std_logic;
        type hc_12_type is array (0 to 1) of std_logic;
        signal hc_12 : hc_12_type;
        signal hc_9 : std_logic;
        signal q2_0 : std_logic;
        signal q1_0 : std_logic;
        signal hc_15 : std_logic_vector(3 downto 0);
        type hc_16_type is array (0 to 15) of std_logic_vector(31 downto 0);
        signal hc_16 : hc_16_type;
        signal hc_14 : std_logic_vector(3 downto 0);
        signal q2_1 : std_logic_vector(31 downto 0);
        signal q1_1 : std_logic_vector(31 downto 0);
        type hc_18_type is array (0 to 127) of std_logic_vector(31 downto 0);
        signal hc_18 : hc_18_type;
        signal hc_19 : std_logic_vector(31 downto 0);

    begin

        hc_11 <= hc_sl(d(0 downto 0));
        hc_10 <= hc_sl(wa(0 downto 0));
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_12(to_integer(hc_uns(hc_10))) <= hc_11;
                end if;
            end if;
        end process;
        hc_9 <= hc_sl(ra(0 downto 0));
        q2_0 <= hc_12(to_integer(hc_uns(hc_9)));
        hc_15 <= wa(3 downto 0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_16(to_integer(hc_uns(hc_15))) <= d;
                end if;
            end if;
        end process;
        hc_14 <= ra(3 downto 0);
        q2_1 <= hc_16(to_integer(hc_uns(hc_14)));
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_18(to_integer(hc_uns(wa))) <= d;
                end if;
            end if;
        end process;
        hc_19 <= hc_18(to_integer(hc_uns(ra)));
        q1_0 <= q2_0;
        q1_1 <= q2_1;
        q1 <= hc_19;
        q2 <= q2_1;
        q3 <= q2_0;

    end architecture;
    |}]
;;
