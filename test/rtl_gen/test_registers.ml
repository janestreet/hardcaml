open Hardcaml

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let reset = Signal.input "reset" 1
let enable = Signal.input "enable" 1
let d = Signal.input "d" 1

let of_spec ?initialize_to ?reset_to ?clear_to ?enable spec =
  Circuit.create_exn
    ~name:"my_register"
    [ Signal.output "q" (Signal.reg spec ?enable ?initialize_to ?reset_to ?clear_to d) ]
;;

let%expect_test "clock" =
  let spec = Signal.Reg_spec.create ~clock () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec);
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

        reg _4;
        always @(posedge clock) begin
            _4 <= d;
        end
        assign q = _4;

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

        signal \_4\ : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                \_4\ <= d;
            end if;
        end process;
        q <= \_4\;

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

        reg _4;
        always @(posedge clock) begin
            _4 <= d;
        end
        assign q = _4;

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

        signal \_4\ : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                \_4\ <= d;
            end if;
        end process;
        q <= \_4\;

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

        reg _5;
        always @(posedge clock) begin
            if (enable)
                _5 <= d;
        end
        assign q = _5;

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

        signal \_5\ : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                if enable = '1' then
                    \_5\ <= d;
                end if;
            end if;
        end process;
        q <= \_5\;

    end architecture;
    |}];
  let spec = Signal.Reg_spec.override spec ~clock_edge:Falling in
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

        reg _5;
        always @(negedge clock) begin
            if (enable)
                _5 <= d;
        end
        assign q = _5;

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

        signal \_5\ : std_logic;

    begin

        process (clock) begin
            if falling_edge(clock) then
                if enable = '1' then
                    \_5\ <= d;
                end if;
            end if;
        end process;
        q <= \_5\;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog
    ~show:true
    (of_spec spec ~initialize_to:(Bits.of_string "1"));
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

        reg _4 = 1'b1;
        always @(negedge clock) begin
            _4 <= d;
        end
        assign q = _4;

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

        signal \_4\ : std_logic := '1';

    begin

        process (clock) begin
            if falling_edge(clock) then
                \_4\ <= d;
            end if;
        end process;
        q <= \_4\;

    end architecture;
    |}]
;;

let%expect_test "empty enable raises" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    let spec = Signal.Reg_spec.create ~clock ~reset () in
    of_spec spec ~enable:Signal.empty);
  [%expect
    {|
    ("enable is invalid"
      (info           "signal has unexpected width")
      (expected_width 1)
      (signal         empty))
    |}]
;;

let%expect_test "clock + reset" =
  let spec = Signal.Reg_spec.create ~clock ~reset () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec);
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

        wire _5;
        reg _6;
        assign _5 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _6 <= _5;
            else
                _6 <= d;
        end
        assign q = _6;

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

        signal \_5\ : std_logic;
        signal \_6\ : std_logic;

    begin

        \_5\ <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                \_6\ <= \_5\;
            else
                if rising_edge(clock) then
                    \_6\ <= d;
                end if;
            end if;
        end process;
        q <= \_6\;

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

        wire _5;
        reg _6;
        assign _5 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _6 <= _5;
            else
                _6 <= d;
        end
        assign q = _6;

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

        signal \_5\ : std_logic;
        signal \_6\ : std_logic;

    begin

        \_5\ <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                \_6\ <= \_5\;
            else
                if rising_edge(clock) then
                    \_6\ <= d;
                end if;
            end if;
        end process;
        q <= \_6\;

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

        wire _6;
        reg _7;
        assign _6 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _7 <= _6;
            else
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
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

        signal \_6\ : std_logic;
        signal \_7\ : std_logic;

    begin

        \_6\ <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                \_7\ <= \_6\;
            else
                if rising_edge(clock) then
                    if enable = '1' then
                        \_7\ <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= \_7\;

    end architecture;
    |}];
  let spec = Signal.Reg_spec.override spec ~reset_edge:Falling in
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

        wire _6;
        reg _7;
        assign _6 = 1'b0;
        always @(posedge clock or negedge reset) begin
            if (reset == 0)
                _7 <= _6;
            else
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
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

        signal \_6\ : std_logic;
        signal \_7\ : std_logic;

    begin

        \_6\ <= '0';
        process (clock, reset) begin
            if falling_edge(reset) then
                \_7\ <= \_6\;
            else
                if rising_edge(clock) then
                    if enable = '1' then
                        \_7\ <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= \_7\;

    end architecture;
    |}]
;;

let%expect_test "clock + clear" =
  let spec = Signal.Reg_spec.create ~clock ~clear () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec);
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

        wire _5;
        reg _6;
        assign _5 = 1'b0;
        always @(posedge clock) begin
            if (clear)
                _6 <= _5;
            else
                _6 <= d;
        end
        assign q = _6;

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

        signal \_5\ : std_logic;
        signal \_6\ : std_logic;

    begin

        \_5\ <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    \_6\ <= \_5\;
                else
                    \_6\ <= d;
                end if;
            end if;
        end process;
        q <= \_6\;

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

        wire _5;
        reg _6;
        assign _5 = 1'b0;
        always @(posedge clock) begin
            if (clear)
                _6 <= _5;
            else
                _6 <= d;
        end
        assign q = _6;

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

        signal \_5\ : std_logic;
        signal \_6\ : std_logic;

    begin

        \_5\ <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    \_6\ <= \_5\;
                else
                    \_6\ <= d;
                end if;
            end if;
        end process;
        q <= \_6\;

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

        wire _6;
        reg _7;
        assign _6 = 1'b0;
        always @(posedge clock) begin
            if (clear)
                _7 <= _6;
            else
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
            clear : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

        signal \_6\ : std_logic;
        signal \_7\ : std_logic;

    begin

        \_6\ <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    \_7\ <= \_6\;
                else
                    if enable = '1' then
                        \_7\ <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= \_7\;

    end architecture;
    |}]
;;

let%expect_test "clock + reset + clear" =
  let spec = Signal.Reg_spec.create ~clock ~reset ~clear () in
  Testing.analyse_vhdl_and_verilog ~show:true (of_spec spec);
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

        wire _7;
        reg _8;
        assign _7 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _8 <= _7;
            else
                if (clear)
                    _8 <= _7;
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
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

        signal \_7\ : std_logic;
        signal \_8\ : std_logic;

    begin

        \_7\ <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                \_8\ <= \_7\;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        \_8\ <= \_7\;
                    else
                        \_8\ <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= \_8\;

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

        wire _7;
        reg _8;
        assign _7 = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                _8 <= _7;
            else
                if (clear)
                    _8 <= _7;
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
            reset : in std_logic;
            clock : in std_logic;
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of my_register is

        signal \_7\ : std_logic;
        signal \_8\ : std_logic;

    begin

        \_7\ <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                \_8\ <= \_7\;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        \_8\ <= \_7\;
                    else
                        \_8\ <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= \_8\;

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

        signal \_8\ : std_logic;
        signal \_9\ : std_logic;

    begin

        \_8\ <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                \_9\ <= \_8\;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        \_9\ <= \_8\;
                    else
                        if enable = '1' then
                            \_9\ <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= \_9\;

    end architecture;
    |}];
  let spec = Signal.Reg_spec.override spec ~clock_edge:Falling ~reset_edge:Falling in
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

        signal \_8\ : std_logic;
        signal \_9\ : std_logic;

    begin

        \_8\ <= '0';
        process (clock, reset) begin
            if falling_edge(reset) then
                \_9\ <= \_8\;
            else
                if falling_edge(clock) then
                    if clear = '1' then
                        \_9\ <= \_8\;
                    else
                        if enable = '1' then
                            \_9\ <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= \_9\;

    end architecture;
    |}];
  Testing.analyse_vhdl_and_verilog
    ~show:true
    (of_spec spec ~enable ~reset_to:Bits.vdd ~clear_to:Signal.vdd);
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
        wire _7;
        reg _9;
        assign vdd = 1'b1;
        assign _7 = 1'b1;
        always @(negedge clock or negedge reset) begin
            if (reset == 0)
                _9 <= _7;
            else
                if (clear)
                    _9 <= vdd;
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

        signal vdd : std_logic;
        signal \_7\ : std_logic;
        signal \_9\ : std_logic;

    begin

        vdd <= '1';
        \_7\ <= '1';
        process (clock, reset) begin
            if falling_edge(reset) then
                \_9\ <= \_7\;
            else
                if falling_edge(clock) then
                    if clear = '1' then
                        \_9\ <= vdd;
                    else
                        if enable = '1' then
                            \_9\ <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= \_9\;

    end architecture;
    |}]
;;

let%expect_test "multiple reg names" =
  (* ensure we are generating the name aliases correctly (in verilog they should be wires
     not regs) *)
  let spec = Signal.Reg_spec.create ~clock ~reset ~clear () in
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

        signal \_8\ : std_logic;
        signal c : std_logic;
        signal b : std_logic;
        signal a : std_logic;

    begin

        \_8\ <= '0';
        process (clock, reset) begin
            if rising_edge(reset) then
                c <= \_8\;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        c <= \_8\;
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
