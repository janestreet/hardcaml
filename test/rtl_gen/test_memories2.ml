open! Base
open Hardcaml

let circuit ~data_width ~address_width =
  let open Signal in
  let clock = input "clock" 1 in
  let write_data = input "d" data_width in
  let write_address = input "wa" address_width in
  let write_enable = input "we" 1 in
  let read_address = input "ra" address_width in
  Circuit.create_exn
    ~name:"multimem"
    (multiport_memory
       (1 lsl address_width)
       ~write_ports:[| { write_clock = clock; write_address; write_enable; write_data } |]
       ~read_addresses:[| read_address |]
     |> Array.to_list
     |> List.mapi ~f:(fun i q -> output ("q" ^ Int.to_string i) q))
;;

let%expect_test "multiport memorydata" =
  Testing.analyse_vhdl_and_verilog ~show:true (circuit ~data_width:32 ~address_width:7);
  [%expect
    {|
    module multimem (
        we,
        d,
        wa,
        clock,
        ra,
        q0
    );

        input we;
        input [31:0] d;
        input [6:0] wa;
        input clock;
        input [6:0] ra;
        output [31:0] q0;

        reg [31:0] _7[0:127];
        wire [31:0] _8;
        always @(posedge clock) begin
            if (we)
                _7[wa] <= d;
        end
        assign _8 = _7[ra];
        assign q0 = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity multimem is
        port (
            we : in std_logic;
            d : in std_logic_vector(31 downto 0);
            wa : in std_logic_vector(6 downto 0);
            clock : in std_logic;
            ra : in std_logic_vector(6 downto 0);
            q0 : out std_logic_vector(31 downto 0)
        );
    end entity;

    architecture rtl of multimem is

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
        type hc_7_type is array (0 to 127) of std_logic_vector(31 downto 0);
        signal hc_7 : hc_7_type;
        signal hc_8 : std_logic_vector(31 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_7(to_integer(hc_uns(wa))) <= d;
                end if;
            end if;
        end process;
        hc_8 <= hc_7(to_integer(hc_uns(ra)));
        q0 <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (circuit ~data_width:15 ~address_width:1);
  [%expect
    {|
    module multimem (
        we,
        d,
        wa,
        clock,
        ra,
        q0
    );

        input we;
        input [14:0] d;
        input wa;
        input clock;
        input ra;
        output [14:0] q0;

        reg [14:0] _7[0:1];
        wire [14:0] _8;
        always @(posedge clock) begin
            if (we)
                _7[wa] <= d;
        end
        assign _8 = _7[ra];
        assign q0 = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity multimem is
        port (
            we : in std_logic;
            d : in std_logic_vector(14 downto 0);
            wa : in std_logic;
            clock : in std_logic;
            ra : in std_logic;
            q0 : out std_logic_vector(14 downto 0)
        );
    end entity;

    architecture rtl of multimem is

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
        type hc_7_type is array (0 to 1) of std_logic_vector(14 downto 0);
        signal hc_7 : hc_7_type;
        signal hc_8 : std_logic_vector(14 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_7(to_integer(hc_uns(wa))) <= d;
                end if;
            end if;
        end process;
        hc_8 <= hc_7(to_integer(hc_uns(ra)));
        q0 <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (circuit ~data_width:1 ~address_width:2);
  [%expect
    {|
    module multimem (
        we,
        d,
        wa,
        clock,
        ra,
        q0
    );

        input we;
        input d;
        input [1:0] wa;
        input clock;
        input [1:0] ra;
        output q0;

        reg [0:0] _7[0:3];
        wire _8;
        always @(posedge clock) begin
            if (we)
                _7[wa] <= d;
        end
        assign _8 = _7[ra];
        assign q0 = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity multimem is
        port (
            we : in std_logic;
            d : in std_logic;
            wa : in std_logic_vector(1 downto 0);
            clock : in std_logic;
            ra : in std_logic_vector(1 downto 0);
            q0 : out std_logic
        );
    end entity;

    architecture rtl of multimem is

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
        type hc_7_type is array (0 to 3) of std_logic;
        signal hc_7 : hc_7_type;
        signal hc_8 : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_7(to_integer(hc_uns(wa))) <= d;
                end if;
            end if;
        end process;
        hc_8 <= hc_7(to_integer(hc_uns(ra)));
        q0 <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (circuit ~data_width:1 ~address_width:1);
  [%expect
    {|
    module multimem (
        we,
        d,
        wa,
        clock,
        ra,
        q0
    );

        input we;
        input d;
        input wa;
        input clock;
        input ra;
        output q0;

        reg [0:0] _7[0:1];
        wire _8;
        always @(posedge clock) begin
            if (we)
                _7[wa] <= d;
        end
        assign _8 = _7[ra];
        assign q0 = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity multimem is
        port (
            we : in std_logic;
            d : in std_logic;
            wa : in std_logic;
            clock : in std_logic;
            ra : in std_logic;
            q0 : out std_logic
        );
    end entity;

    architecture rtl of multimem is

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
        type hc_7_type is array (0 to 1) of std_logic;
        signal hc_7 : hc_7_type;
        signal hc_8 : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    hc_7(to_integer(hc_uns(wa))) <= d;
                end if;
            end if;
        end process;
        hc_8 <= hc_7(to_integer(hc_uns(ra)));
        q0 <= hc_8;

    end architecture;
    |}]
;;
