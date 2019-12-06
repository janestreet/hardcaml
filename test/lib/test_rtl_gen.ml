open! Import
open Signal

let d = input "d" 8

let%expect_test "reg, clock + enable" =
  let spec = Reg_spec.create () ~clock in
  let q = reg spec ~enable d in
  let circuit = Circuit.create_exn ~name:"reg" [ output "q" q ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module reg (
        enable,
        clock,
        d,
        q
    );

        input enable;
        input clock;
        input [7:0] d;
        output [7:0] q;

        /* signal declarations */
        wire [7:0] _6 = 8'b00000000;
        wire [7:0] _5 = 8'b00000000;
        reg [7:0] _7;

        /* logic */
        always @(posedge clock) begin
            if (enable)
                _7 <= d;
        end

        /* aliases */

        /* output assignments */
        assign q = _7;

    endmodule |}];
  Rtl.print Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity reg is
        port (
            enable : in std_logic;
            clock : in std_logic;
            d : in std_logic_vector (7 downto 0);
            q : out std_logic_vector (7 downto 0)
        );
    end entity;

    architecture rtl of reg is

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

        -- signal declarations
        constant hc_6 : std_logic_vector (7 downto 0) := "00000000";
        constant hc_5 : std_logic_vector (7 downto 0) := "00000000";
        signal hc_7 : std_logic_vector (7 downto 0);

    begin

        -- logic
        process (clock) begin
            if rising_edge(clock) then
                if enable = '1' then
                    hc_7 <= d;
                end if;
            end if;
        end process;

        -- aliases

        -- output assignments
        q <= hc_7;

    end architecture; |}]
;;

let%expect_test "reg, clock, reset, clear + enable" =
  let spec = Reg_spec.create () ~clock ~clear ~reset in
  let q = reg spec ~enable d in
  let circuit = Circuit.create_exn ~name:"reg" [ output "q" q ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module reg (
        enable,
        clear,
        reset,
        clock,
        d,
        q
    );

        input enable;
        input clear;
        input reset;
        input clock;
        input [7:0] d;
        output [7:0] q;

        /* signal declarations */
        wire [7:0] _8 = 8'b00000000;
        wire [7:0] _7 = 8'b00000000;
        reg [7:0] _9;

        /* logic */
        always @(posedge clock or posedge reset) begin
            if (reset)
                _9 <= _7;
            else
                if (clear)
                    _9 <= _8;
                else
                    if (enable)
                        _9 <= d;
        end

        /* aliases */

        /* output assignments */
        assign q = _9;

    endmodule |}];
  Rtl.print Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity reg is
        port (
            enable : in std_logic;
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic_vector (7 downto 0);
            q : out std_logic_vector (7 downto 0)
        );
    end entity;

    architecture rtl of reg is

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

        -- signal declarations
        constant hc_8 : std_logic_vector (7 downto 0) := "00000000";
        constant hc_7 : std_logic_vector (7 downto 0) := "00000000";
        signal hc_9 : std_logic_vector (7 downto 0);

    begin

        -- logic
        process (clock, reset) begin
            if rising_edge(reset) then
                hc_9 <= hc_7;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        hc_9 <= hc_8;
                    else
                        if enable = '1' then
                            hc_9 <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;

        -- aliases

        -- output assignments
        q <= hc_9;

    end architecture; |}]
;;

let write_data = input "write_data" 8
let write_enable = input "write_enable" 1
let write_address = input "write_address" 2
let read_address = input "read_address" 2
let read_enable = input "read_enable" 1

let%expect_test "mem" =
  let q =
    memory
      4
      ~write_port:{ write_clock = clock; write_address; write_data; write_enable }
      ~read_address
  in
  let circuit = Circuit.create_exn ~name:"reg" [ output "q" q ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module reg (
        clock,
        write_enable,
        read_address,
        write_address,
        write_data,
        q
    );

        input clock;
        input write_enable;
        input [1:0] read_address;
        input [1:0] write_address;
        input [7:0] write_data;
        output [7:0] q;

        /* signal declarations */
        wire [7:0] _8 = 8'b00000000;
        wire [7:0] _7 = 8'b00000000;
        wire [7:0] _10;
        reg [7:0] _10_mem[0:3];

        /* logic */
        always @(posedge clock) begin
            if (write_enable)
                _10_mem[write_address] <= write_data;
        end
        assign _10 = _10_mem[read_address];

        /* aliases */

        /* output assignments */
        assign q = _10;

    endmodule |}];
  Rtl.print Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity reg is
        port (
            clock : in std_logic;
            write_enable : in std_logic;
            read_address : in std_logic_vector (1 downto 0);
            write_address : in std_logic_vector (1 downto 0);
            write_data : in std_logic_vector (7 downto 0);
            q : out std_logic_vector (7 downto 0)
        );
    end entity;

    architecture rtl of reg is

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

        -- signal declarations
        constant hc_8 : std_logic_vector (7 downto 0) := "00000000";
        constant hc_7 : std_logic_vector (7 downto 0) := "00000000";
        signal hc_10 : std_logic_vector (7 downto 0);
        type hc_10_type is array (0 to 3) of std_logic_vector(7 downto 0);
        signal hc_10_mem : hc_10_type;

    begin

        -- logic
        process (clock) begin
            if rising_edge(clock) then
                if write_enable = '1' then
                    hc_10_mem(to_integer(hc_uns(write_address))) <= write_data;
                end if;
            end if;
        end process;
        hc_10 <= hc_10_mem(to_integer(hc_uns(read_address)));

        -- aliases

        -- output assignments
        q <= hc_10;

    end architecture; |}]
;;

let%expect_test "multiport mem" =
  let q =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:4
      ~write_ports:[| { write_clock = clock; write_address; write_enable; write_data } |]
      ~read_ports:[| { read_clock = clock; read_address; read_enable } |]
  in
  let q =
    Array.to_list q |> List.mapi ~f:(fun i q -> output ("q" ^ Int.to_string i) q)
  in
  let circuit = Circuit.create_exn ~name:"reg" q in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module reg (
        read_enable,
        write_enable,
        write_data,
        write_address,
        clock,
        read_address,
        q0
    );

        input read_enable;
        input write_enable;
        input [7:0] write_data;
        input [1:0] write_address;
        input clock;
        input [1:0] read_address;
        output [7:0] q0;

        /* signal declarations */
        wire [7:0] _11 = 8'b00000000;
        wire [7:0] _10 = 8'b00000000;
        reg [7:0] _8[0:3];
        wire [7:0] _9;
        reg [7:0] _12;

        /* logic */
        always @(posedge clock) begin
            if (write_enable)
                _8[write_address] <= write_data;
        end
        assign _9 = _8[read_address];
        always @(posedge clock) begin
            if (read_enable)
                _12 <= _9;
        end

        /* aliases */

        /* output assignments */
        assign q0 = _12;

    endmodule |}];
  Rtl.print Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity reg is
        port (
            read_enable : in std_logic;
            write_enable : in std_logic;
            write_data : in std_logic_vector (7 downto 0);
            write_address : in std_logic_vector (1 downto 0);
            clock : in std_logic;
            read_address : in std_logic_vector (1 downto 0);
            q0 : out std_logic_vector (7 downto 0)
        );
    end entity;

    architecture rtl of reg is

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

        -- signal declarations
        constant hc_11 : std_logic_vector (7 downto 0) := "00000000";
        constant hc_10 : std_logic_vector (7 downto 0) := "00000000";
        type hc_8_type is array (0 to 3) of std_logic_vector(7 downto 0);
        signal hc_8 : hc_8_type;
        signal hc_9 : std_logic_vector (7 downto 0);
        signal hc_12 : std_logic_vector (7 downto 0);

    begin

        -- logic
        process (clock) begin
            if rising_edge(clock) then
                if write_enable = '1' then
                    hc_8(to_integer(hc_uns(write_address))) <= write_data;
                end if;
            end if;
        end process;
        hc_9 <= hc_8(to_integer(hc_uns(read_address)));
        process (clock) begin
            if rising_edge(clock) then
                if read_enable = '1' then
                    hc_12 <= hc_9;
                end if;
            end if;
        end process;

        -- aliases

        -- output assignments
        q0 <= hc_12;

    end architecture; |}]
;;
