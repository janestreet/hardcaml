open Hardcaml

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let reset = Signal.input "reset" 1
let enable = Signal.input "enable" 1
let d = Signal.input "d" 1

let of_spec spec ~enable =
  Circuit.create_exn ~name:"my_register" [ Signal.output "q" (Signal.reg spec ~enable d) ]
;;

let%expect_test "clock" =
  let spec = Reg_spec.create ~clock () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.empty);
  [%expect
    {|
    module my_register (
        clock,
        d,
        q
    );

        input clock;
        input d;
        output q;

        wire vdd;
        wire _5;
        reg _7;
        assign vdd = 1'b1;
        assign _5 = 1'b0;
        always @(posedge clock) begin
            _7 <= d;
        end
        assign q = _7;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_5 : std_logic;
        signal hc_7 : std_logic;

    begin

        vdd <= '1';
        hc_5 <= '0';
        process (clock) begin
            if rising_edge(clock) then
                hc_7 <= d;
            end if;
        end process;
        q <= hc_7;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.vdd);
  [%expect
    {|
    module my_register (
        clock,
        d,
        q
    );

        input clock;
        input d;
        output q;

        wire vdd;
        wire _5;
        reg _7;
        assign vdd = 1'b1;
        assign _5 = 1'b0;
        always @(posedge clock) begin
            _7 <= d;
        end
        assign q = _7;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_5 : std_logic;
        signal hc_7 : std_logic;

    begin

        vdd <= '1';
        hc_5 <= '0';
        process (clock) begin
            if rising_edge(clock) then
                hc_7 <= d;
            end if;
        end process;
        q <= hc_7;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
        enable,
        clock,
        d,
        q
    );

        input enable;
        input clock;
        input d;
        output q;

        wire _6;
        reg _7;
        assign _6 = 1'b0;
        always @(posedge clock) begin
            if (enable)
                _7 <= d;
        end
        assign q = _7;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_6 : std_logic;
        signal hc_7 : std_logic;

    begin

        hc_6 <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if enable = '1' then
                    hc_7 <= d;
                end if;
            end if;
        end process;
        q <= hc_7;

    end architecture;
    |}];
  let spec = Reg_spec.override spec ~clock_edge:Falling in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
        enable,
        clock,
        d,
        q
    );

        input enable;
        input clock;
        input d;
        output q;

        wire _6;
        reg _7;
        assign _6 = 1'b0;
        always @(negedge clock) begin
            if (enable)
                _7 <= d;
        end
        assign q = _7;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_6 : std_logic;
        signal hc_7 : std_logic;

    begin

        hc_6 <= '0';
        process (clock) begin
            if falling_edge(clock) then
                if enable = '1' then
                    hc_7 <= d;
                end if;
            end if;
        end process;
        q <= hc_7;

    end architecture;
    |}]
;;

let%expect_test "clock + reset" =
  let spec = Reg_spec.create ~clock ~reset () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.empty);
  [%expect
    {|
    module my_register (
        reset,
        clock,
        d,
        q
    );

        input reset;
        input clock;
        input d;
        output q;

        wire vdd;
        wire _6;
        reg _8;
        assign vdd = 1'b1;
        assign _6 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _8 <= _6;
            else
                _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_6 : std_logic;
        signal hc_8 : std_logic;

    begin

        vdd <= '1';
        hc_6 <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                hc_8 <= hc_6;
            else
                if rising_edge(clock) then
                    hc_8 <= d;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.vdd);
  [%expect
    {|
    module my_register (
        reset,
        clock,
        d,
        q
    );

        input reset;
        input clock;
        input d;
        output q;

        wire vdd;
        wire _6;
        reg _8;
        assign vdd = 1'b1;
        assign _6 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _8 <= _6;
            else
                _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_6 : std_logic;
        signal hc_8 : std_logic;

    begin

        vdd <= '1';
        hc_6 <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                hc_8 <= hc_6;
            else
                if rising_edge(clock) then
                    hc_8 <= d;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
        enable,
        reset,
        clock,
        d,
        q
    );

        input enable;
        input reset;
        input clock;
        input d;
        output q;

        wire _7;
        reg _8;
        assign _7 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _8 <= _7;
            else
                if (enable)
                    _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_7 : std_logic;
        signal hc_8 : std_logic;

    begin

        hc_7 <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                hc_8 <= hc_7;
            else
                if rising_edge(clock) then
                    if enable = '1' then
                        hc_8 <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}];
  let spec = Reg_spec.override spec ~reset_edge:Falling in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
        enable,
        reset,
        clock,
        d,
        q
    );

        input enable;
        input reset;
        input clock;
        input d;
        output q;

        wire _7;
        reg _8;
        assign _7 = 1'b0;
        always @(posedge clock or negedge reset) begin
            if (reset == 0)
                _8 <= _7;
            else
                if (enable)
                    _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_7 : std_logic;
        signal hc_8 : std_logic;

    begin

        hc_7 <= '0';
        process (clock, reset) begin
            if falling_edge(reset) then
                hc_8 <= hc_7;
            else
                if rising_edge(clock) then
                    if enable = '1' then
                        hc_8 <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}]
;;

let%expect_test "clock + clear" =
  let spec = Reg_spec.create ~clock ~clear () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.empty);
  [%expect
    {|
    module my_register (
        clear,
        clock,
        d,
        q
    );

        input clear;
        input clock;
        input d;
        output q;

        wire vdd;
        wire _6;
        reg _8;
        assign vdd = 1'b1;
        assign _6 = 1'b0;
        always @(posedge clock) begin
            if (clear)
                _8 <= _6;
            else
                _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            clear : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_6 : std_logic;
        signal hc_8 : std_logic;

    begin

        vdd <= '1';
        hc_6 <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    hc_8 <= hc_6;
                else
                    hc_8 <= d;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.vdd);
  [%expect
    {|
    module my_register (
        clear,
        clock,
        d,
        q
    );

        input clear;
        input clock;
        input d;
        output q;

        wire vdd;
        wire _6;
        reg _8;
        assign vdd = 1'b1;
        assign _6 = 1'b0;
        always @(posedge clock) begin
            if (clear)
                _8 <= _6;
            else
                _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            clear : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_6 : std_logic;
        signal hc_8 : std_logic;

    begin

        vdd <= '1';
        hc_6 <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    hc_8 <= hc_6;
                else
                    hc_8 <= d;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
        enable,
        clear,
        clock,
        d,
        q
    );

        input enable;
        input clear;
        input clock;
        input d;
        output q;

        wire _7;
        reg _8;
        assign _7 = 1'b0;
        always @(posedge clock) begin
            if (clear)
                _8 <= _7;
            else
                if (enable)
                    _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            clear : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_7 : std_logic;
        signal hc_8 : std_logic;

    begin

        hc_7 <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    hc_8 <= hc_7;
                else
                    if enable = '1' then
                        hc_8 <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}];
  let spec = Reg_spec.override spec ~clear_level:Low in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.empty);
  [%expect
    {|
    module my_register (
        clear,
        clock,
        d,
        q
    );

        input clear;
        input clock;
        input d;
        output q;

        wire vdd;
        wire _6;
        reg _8;
        assign vdd = 1'b1;
        assign _6 = 1'b0;
        always @(posedge clock) begin
            if (clear == 0)
                _8 <= _6;
            else
                _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            clear : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_6 : std_logic;
        signal hc_8 : std_logic;

    begin

        vdd <= '1';
        hc_6 <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '0' then
                    hc_8 <= hc_6;
                else
                    hc_8 <= d;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}]
;;

let%expect_test "clock + reset + clear" =
  let spec = Reg_spec.create ~clock ~reset ~clear () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.empty);
  [%expect
    {|
    module my_register (
        clear,
        reset,
        clock,
        d,
        q
    );

        input clear;
        input reset;
        input clock;
        input d;
        output q;

        wire vdd;
        wire _7;
        reg _9;
        assign vdd = 1'b1;
        assign _7 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _9 <= _7;
            else
                if (clear)
                    _9 <= _7;
                else
                    _9 <= d;
        end
        assign q = _9;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_7 : std_logic;
        signal hc_9 : std_logic;

    begin

        vdd <= '1';
        hc_7 <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                hc_9 <= hc_7;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        hc_9 <= hc_7;
                    else
                        hc_9 <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= hc_9;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable:Signal.vdd);
  [%expect
    {|
    module my_register (
        clear,
        reset,
        clock,
        d,
        q
    );

        input clear;
        input reset;
        input clock;
        input d;
        output q;

        wire vdd;
        wire _7;
        reg _9;
        assign vdd = 1'b1;
        assign _7 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _9 <= _7;
            else
                if (clear)
                    _9 <= _7;
                else
                    _9 <= d;
        end
        assign q = _9;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_7 : std_logic;
        signal hc_9 : std_logic;

    begin

        vdd <= '1';
        hc_7 <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                hc_9 <= hc_7;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        hc_9 <= hc_7;
                    else
                        hc_9 <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= hc_9;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
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
        input d;
        output q;

        wire _8;
        reg _9;
        assign _8 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _9 <= _8;
            else
                if (clear)
                    _9 <= _8;
                else
                    if (enable)
                        _9 <= d;
        end
        assign q = _9;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_8 : std_logic;
        signal hc_9 : std_logic;

    begin

        hc_8 <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                hc_9 <= hc_8;
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
        q <= hc_9;

    end architecture;
    |}];
  let spec =
    Reg_spec.override spec ~clock_edge:Falling ~reset_edge:Falling ~clear_level:Low
  in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
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
        input d;
        output q;

        wire _8;
        reg _9;
        assign _8 = 1'b0;
        always @(negedge clock or negedge reset) begin
            if (reset == 0)
                _9 <= _8;
            else
                if (clear == 0)
                    _9 <= _8;
                else
                    if (enable)
                        _9 <= d;
        end
        assign q = _9;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_8 : std_logic;
        signal hc_9 : std_logic;

    begin

        hc_8 <= '0';
        process (clock, reset) begin
            if falling_edge(reset) then
                hc_9 <= hc_8;
            else
                if falling_edge(clock) then
                    if clear = '0' then
                        hc_9 <= hc_8;
                    else
                        if enable = '1' then
                            hc_9 <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= hc_9;

    end architecture;
    |}];
  let spec = Reg_spec.override spec ~reset_to:Signal.vdd ~clear_to:Signal.vdd in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec ~enable);
  [%expect
    {|
    module my_register (
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
        input d;
        output q;

        wire vdd;
        reg _8;
        assign vdd = 1'b1;
        always @(negedge clock or negedge reset) begin
            if (reset == 0)
                _8 <= vdd;
            else
                if (clear == 0)
                    _8 <= vdd;
                else
                    if (enable)
                        _8 <= d;
        end
        assign q = _8;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal vdd : std_logic;
        signal hc_8 : std_logic;

    begin

        vdd <= '1';
        process (clock, reset) begin
            if falling_edge(reset) then
                hc_8 <= vdd;
            else
                if falling_edge(clock) then
                    if clear = '0' then
                        hc_8 <= vdd;
                    else
                        if enable = '1' then
                            hc_8 <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= hc_8;

    end architecture;
    |}]
;;

let%expect_test "multiple reg names" =
  (* ensure we are generating the name aliases correctly (in verilog they should be wires
     not regs) *)
  let spec = Reg_spec.create ~clock ~reset ~clear () in
  Circuit.create_exn
    ~name:"my_register"
    Signal.[ output "q" (reg spec ~enable d -- "a" -- "b" -- "c") ]
  |> Testing.analyse_vhdl_and_verilog ~show:true;
  [%expect
    {|
    module my_register (
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
        input d;
        output q;

        wire _8;
        reg c;
        wire b;
        wire a;
        assign _8 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                c <= _8;
            else
                if (clear)
                    c <= _8;
                else
                    if (enable)
                        c <= d;
        end
        assign b = c;
        assign a = c;
        assign q = c;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_register is
        port (
            enable : in std_logic;
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

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
        signal hc_8 : std_logic;
        signal c : std_logic;
        signal b : std_logic;
        signal a : std_logic;

    begin

        hc_8 <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                c <= hc_8;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        c <= hc_8;
                    else
                        if enable = '1' then
                            c <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        b <= c;
        a <= c;
        q <= c;

    end architecture;
    |}]
;;
