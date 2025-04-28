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

        signal hc_11 : std_logic;
        signal hc_10 : std_logic;
        type hc_12_type is protected
            procedure set(address : integer; data : std_logic);
            impure function get(address : integer) return std_logic;
        end protected;
        type hc_12_type is protected body
            type t is array (0 to 1) of std_logic;
            variable memory : t;
            procedure set(address : integer; data : std_logic) is begin memory(address) := data; end procedure;
            impure function get(address : integer) return std_logic is begin return memory(address); end function;
        end protected body;
        shared variable hc_12 : hc_12_type;
        signal hc_9 : std_logic;
        signal q2_0 : std_logic;
        signal q1_0 : std_logic;
        signal hc_15 : std_logic_vector(3 downto 0);
        type hc_16_type is protected
            procedure set(address : integer; data : std_logic_vector(31 downto 0));
            impure function get(address : integer) return std_logic_vector;
        end protected;
        type hc_16_type is protected body
            type t is array (0 to 15) of std_logic_vector(31 downto 0);
            variable memory : t;
            procedure set(address : integer; data : std_logic_vector(31 downto 0)) is begin memory(address) := data; end procedure;
            impure function get(address : integer) return std_logic_vector is begin return memory(address); end function;
        end protected body;
        shared variable hc_16 : hc_16_type;
        signal hc_14 : std_logic_vector(3 downto 0);
        signal q2_1 : std_logic_vector(31 downto 0);
        signal q1_1 : std_logic_vector(31 downto 0);
        type hc_18_type is protected
            procedure set(address : integer; data : std_logic_vector(31 downto 0));
            impure function get(address : integer) return std_logic_vector;
        end protected;
        type hc_18_type is protected body
            type t is array (0 to 127) of std_logic_vector(31 downto 0);
            variable memory : t;
            procedure set(address : integer; data : std_logic_vector(31 downto 0)) is begin memory(address) := data; end procedure;
            impure function get(address : integer) return std_logic_vector is begin return memory(address); end function;
        end protected body;
        shared variable hc_18 : hc_18_type;
        signal hc_19 : std_logic_vector(31 downto 0);

    begin

        hc_11 <= d(0);
        hc_10 <= wa(0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_12.set(to_integer(unsigned(std_logic_vector'("" & hc_10))), hc_11);
                end if;
            end if;
        end process;
        hc_9 <= ra(0);
        q2_0 <= hc_12.get(to_integer(unsigned(std_logic_vector'("" & hc_9))));
        hc_15 <= wa(3 downto 0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_16.set(to_integer(unsigned(hc_15)), d);
                end if;
            end if;
        end process;
        hc_14 <= ra(3 downto 0);
        q2_1 <= hc_16.get(to_integer(unsigned(hc_14)));
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_18.set(to_integer(unsigned(wa)), d);
                end if;
            end if;
        end process;
        hc_19 <= hc_18.get(to_integer(unsigned(ra)));
        q1_0 <= q2_0;
        q1_1 <= q2_1;
        q1 <= hc_19;
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

        bit _11;
        bit _10;
        reg [0:0] _12[0:1];
        bit _9;
        bit q2_0;
        bit q1_0;
        bit [3:0] _15;
        reg [31:0] _16[0:15];
        bit [3:0] _14;
        bit [31:0] q2_1;
        bit [31:0] q1_1;
        reg [31:0] _18[0:127];
        bit [31:0] _19;
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
        function to_bit(s : std_ulogic) return bit is begin return to_bit(s, '0'); end;
        function to_bitvector(s : std_ulogic_vector) return bit_vector is begin return to_bitvector(s, '0'); end;

        signal hc_11 : bit;
        signal hc_10 : bit;
        type hc_12_type is protected
            procedure set(address : integer; data : bit);
            impure function get(address : integer) return bit;
        end protected;
        type hc_12_type is protected body
            type t is array (0 to 1) of bit;
            variable memory : t;
            procedure set(address : integer; data : bit) is begin memory(address) := data; end procedure;
            impure function get(address : integer) return bit is begin return memory(address); end function;
        end protected body;
        shared variable hc_12 : hc_12_type;
        signal hc_9 : bit;
        signal q2_0 : bit;
        signal q1_0 : bit;
        signal hc_15 : bit_vector(3 downto 0);
        type hc_16_type is protected
            procedure set(address : integer; data : bit_vector(31 downto 0));
            impure function get(address : integer) return bit_vector;
        end protected;
        type hc_16_type is protected body
            type t is array (0 to 15) of bit_vector(31 downto 0);
            variable memory : t;
            procedure set(address : integer; data : bit_vector(31 downto 0)) is begin memory(address) := data; end procedure;
            impure function get(address : integer) return bit_vector is begin return memory(address); end function;
        end protected body;
        shared variable hc_16 : hc_16_type;
        signal hc_14 : bit_vector(3 downto 0);
        signal q2_1 : bit_vector(31 downto 0);
        signal q1_1 : bit_vector(31 downto 0);
        type hc_18_type is protected
            procedure set(address : integer; data : bit_vector(31 downto 0));
            impure function get(address : integer) return bit_vector;
        end protected;
        type hc_18_type is protected body
            type t is array (0 to 127) of bit_vector(31 downto 0);
            variable memory : t;
            procedure set(address : integer; data : bit_vector(31 downto 0)) is begin memory(address) := data; end procedure;
            impure function get(address : integer) return bit_vector is begin return memory(address); end function;
        end protected body;
        shared variable hc_18 : hc_18_type;
        signal hc_19 : bit_vector(31 downto 0);

    begin

        hc_11 <= d(0);
        hc_10 <= wa(0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_12.set(to_integer(unsigned'("" & hc_10)), hc_11);
                end if;
            end if;
        end process;
        hc_9 <= ra(0);
        q2_0 <= hc_12.get(to_integer(unsigned'("" & hc_9)));
        hc_15 <= wa(3 downto 0);
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_16.set(to_integer(unsigned(hc_15)), d);
                end if;
            end if;
        end process;
        hc_14 <= ra(3 downto 0);
        q2_1 <= hc_16.get(to_integer(unsigned(hc_14)));
        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_18.set(to_integer(unsigned(wa)), d);
                end if;
            end if;
        end process;
        hc_19 <= hc_18.get(to_integer(unsigned(ra)));
        q1_0 <= q2_0;
        q1_1 <= q2_1;
        q1 <= hc_19;
        q2 <= q2_1;
        q3 <= q2_0;

    end architecture;
    |}]
;;
