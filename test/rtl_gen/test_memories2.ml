open! Base
open Hardcaml

let circuit ~initialize ~data_width ~address_width () =
  let open Signal in
  let clock = input "clock" 1 in
  let write_data = input "d" data_width in
  let write_address = input "wa" address_width in
  let write_enable = input "we" 1 in
  let read_address = input "ra" address_width in
  let initialize_to =
    if initialize
    then
      Some
        (Array.init (1 lsl address_width) ~f:(fun i ->
           Bits.of_int_trunc ~width:data_width i))
    else None
  in
  Circuit.create_exn
    ~name:"multimem"
    (multiport_memory
       (1 lsl address_width)
       ~write_ports:[| { write_clock = clock; write_address; write_enable; write_data } |]
       ~read_addresses:[| read_address |]
       ?initialize_to
     |> Array.to_list
     |> List.mapi ~f:(fun i q -> output ("q" ^ Int.to_string i) q))
;;

let%expect_test "multiport memorydata" =
  Testing.analyse_vhdl_and_verilog
    ~show:true
    (circuit ~initialize:false ~data_width:32 ~address_width:7 ());
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
  Testing.analyse_vhdl_and_verilog
    ~show:true
    (circuit ~initialize:true ~data_width:15 ~address_width:1 ());
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
        initial begin
            _7[0] <= 15'b000000000000000;
            _7[1] <= 15'b000000000000001;
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
        process begin
            hc_7(0) <= "000000000000000";
            hc_7(1) <= "000000000000001";
            wait;
        end process;
        hc_8 <= hc_7(to_integer(hc_uns(ra)));
        q0 <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog
    ~show:true
    (circuit ~initialize:true ~data_width:1 ~address_width:2 ());
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
        initial begin
            _7[0] <= 1'b0;
            _7[1] <= 1'b1;
            _7[2] <= 1'b0;
            _7[3] <= 1'b1;
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
        process begin
            hc_7(0) <= '0';
            hc_7(1) <= '1';
            hc_7(2) <= '0';
            hc_7(3) <= '1';
            wait;
        end process;
        hc_8 <= hc_7(to_integer(hc_uns(ra)));
        q0 <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog
    ~show:true
    (circuit ~initialize:true ~data_width:1 ~address_width:1 ());
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
        initial begin
            _7[0] <= 1'b0;
            _7[1] <= 1'b1;
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
        process begin
            hc_7(0) <= '0';
            hc_7(1) <= '1';
            wait;
        end process;
        hc_8 <= hc_7(to_integer(hc_uns(ra)));
        q0 <= hc_8;

    end architecture;
    |}]
;;

let%expect_test "rom - write port is filtered out" =
  let address_width = 4 in
  let data_width = 32 in
  let memory_size = 1 lsl address_width in
  let read_address = Signal.input "read_address" address_width in
  let read_data =
    Signal.rom
      ~read_addresses:[| read_address; Signal.( +:. ) read_address 1 |]
      (Array.init memory_size ~f:(fun i -> Bits.of_int ~width:data_width i))
  in
  let circuit =
    Circuit.create_exn
      ~name:"rom"
      (List.mapi (Array.to_list read_data) ~f:(fun i q ->
         Signal.output ("q" ^ Int.to_string i) q))
  in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module rom (
        read_address,
        q0,
        q1
    );

        input [3:0] read_address;
        output [31:0] q0;
        output [31:0] q1;

        wire [3:0] _4;
        wire [3:0] _5;
        wire [31:0] _7;
        reg [31:0] _6[0:15];
        wire [31:0] _8;
        assign _4 = 4'b0001;
        assign _5 = read_address + _4;
        assign _7 = _6[_5];
        initial begin
            _6[0] <= 32'b00000000000000000000000000000000;
            _6[1] <= 32'b00000000000000000000000000000001;
            _6[2] <= 32'b00000000000000000000000000000010;
            _6[3] <= 32'b00000000000000000000000000000011;
            _6[4] <= 32'b00000000000000000000000000000100;
            _6[5] <= 32'b00000000000000000000000000000101;
            _6[6] <= 32'b00000000000000000000000000000110;
            _6[7] <= 32'b00000000000000000000000000000111;
            _6[8] <= 32'b00000000000000000000000000001000;
            _6[9] <= 32'b00000000000000000000000000001001;
            _6[10] <= 32'b00000000000000000000000000001010;
            _6[11] <= 32'b00000000000000000000000000001011;
            _6[12] <= 32'b00000000000000000000000000001100;
            _6[13] <= 32'b00000000000000000000000000001101;
            _6[14] <= 32'b00000000000000000000000000001110;
            _6[15] <= 32'b00000000000000000000000000001111;
        end
        assign _8 = _6[read_address];
        assign q0 = _8;
        assign q1 = _7;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity rom is
        port (
            read_address : in std_logic_vector(3 downto 0);
            q0 : out std_logic_vector(31 downto 0);
            q1 : out std_logic_vector(31 downto 0)
        );
    end entity;

    architecture rtl of rom is

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
        signal hc_4 : std_logic_vector(3 downto 0);
        signal hc_5 : std_logic_vector(3 downto 0);
        signal hc_7 : std_logic_vector(31 downto 0);
        type hc_6_type is array (0 to 15) of std_logic_vector(31 downto 0);
        signal hc_6 : hc_6_type;
        signal hc_8 : std_logic_vector(31 downto 0);

    begin

        hc_4 <= "0001";
        hc_5 <= hc_slv(hc_uns(read_address) + hc_uns(hc_4));
        hc_7 <= hc_6(to_integer(hc_uns(hc_5)));
        process begin
            hc_6(0) <= "00000000000000000000000000000000";
            hc_6(1) <= "00000000000000000000000000000001";
            hc_6(2) <= "00000000000000000000000000000010";
            hc_6(3) <= "00000000000000000000000000000011";
            hc_6(4) <= "00000000000000000000000000000100";
            hc_6(5) <= "00000000000000000000000000000101";
            hc_6(6) <= "00000000000000000000000000000110";
            hc_6(7) <= "00000000000000000000000000000111";
            hc_6(8) <= "00000000000000000000000000001000";
            hc_6(9) <= "00000000000000000000000000001001";
            hc_6(10) <= "00000000000000000000000000001010";
            hc_6(11) <= "00000000000000000000000000001011";
            hc_6(12) <= "00000000000000000000000000001100";
            hc_6(13) <= "00000000000000000000000000001101";
            hc_6(14) <= "00000000000000000000000000001110";
            hc_6(15) <= "00000000000000000000000000001111";
            wait;
        end process;
        hc_8 <= hc_6(to_integer(hc_uns(read_address)));
        q0 <= hc_8;
        q1 <= hc_7;

    end architecture;
    |}]
;;
