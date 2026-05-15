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

        reg signal_reg;
        always @(posedge clock) begin
            signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_reg : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                signal_reg <= d;
            end if;
        end process;
        q <= signal_reg;

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

        reg signal_reg;
        always @(posedge clock) begin
            signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_reg : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                signal_reg <= d;
            end if;
        end process;
        q <= signal_reg;

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

        reg signal_reg;
        always @(posedge clock) begin
            if (enable)
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_reg : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                if enable = '1' then
                    signal_reg <= d;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        reg signal_reg;
        always @(negedge clock) begin
            if (enable)
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_reg : std_logic;

    begin

        process (clock) begin
            if falling_edge(clock) then
                if enable = '1' then
                    signal_reg <= d;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        reg signal_reg = 1'b1;
        always @(negedge clock) begin
            signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_reg : std_logic := '1';

    begin

        process (clock) begin
            if falling_edge(clock) then
                signal_reg <= d;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                signal_reg <= signal_const;
            else
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '1' then
                signal_reg <= signal_const;
            else
                if rising_edge(clock) then
                    signal_reg <= d;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                signal_reg <= signal_const;
            else
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '1' then
                signal_reg <= signal_const;
            else
                if rising_edge(clock) then
                    signal_reg <= d;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                signal_reg <= signal_const;
            else
                if (enable)
                    signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '1' then
                signal_reg <= signal_const;
            else
                if rising_edge(clock) then
                    if enable = '1' then
                        signal_reg <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

    end architecture;
    |}];
  let spec = Signal.Reg_spec.override spec ~reset_level:Low in
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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock or negedge reset) begin
            if (reset == 0)
                signal_reg <= signal_const;
            else
                if (enable)
                    signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '0' then
                signal_reg <= signal_const;
            else
                if rising_edge(clock) then
                    if enable = '1' then
                        signal_reg <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock) begin
            if (clear)
                signal_reg <= signal_const;
            else
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    signal_reg <= signal_const;
                else
                    signal_reg <= d;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock) begin
            if (clear)
                signal_reg <= signal_const;
            else
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    signal_reg <= signal_const;
                else
                    signal_reg <= d;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock) begin
            if (clear)
                signal_reg <= signal_const;
            else
                if (enable)
                    signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock) begin
            if rising_edge(clock) then
                if clear = '1' then
                    signal_reg <= signal_const;
                else
                    if enable = '1' then
                        signal_reg <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                signal_reg <= signal_const;
            else
                if (clear)
                    signal_reg <= signal_const;
                else
                    signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '1' then
                signal_reg <= signal_const;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        signal_reg <= signal_const;
                    else
                        signal_reg <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                signal_reg <= signal_const;
            else
                if (clear)
                    signal_reg <= signal_const;
                else
                    signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '1' then
                signal_reg <= signal_const;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        signal_reg <= signal_const;
                    else
                        signal_reg <= d;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                signal_reg <= signal_const;
            else
                if (clear)
                    signal_reg <= signal_const;
                else
                    if (enable)
                        signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '1' then
                signal_reg <= signal_const;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        signal_reg <= signal_const;
                    else
                        if enable = '1' then
                            signal_reg <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

    end architecture;
    |}];
  let spec = Signal.Reg_spec.override spec ~clock_edge:Falling ~reset_level:Low in
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

        wire signal_const;
        reg signal_reg;
        assign signal_const = 1'b0;
        always @(negedge clock or negedge reset) begin
            if (reset == 0)
                signal_reg <= signal_const;
            else
                if (clear)
                    signal_reg <= signal_const;
                else
                    if (enable)
                        signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '0' then
                signal_reg <= signal_const;
            else
                if falling_edge(clock) then
                    if clear = '1' then
                        signal_reg <= signal_const;
                    else
                        if enable = '1' then
                            signal_reg <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

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
        wire signal_const;
        reg signal_reg;
        assign vdd = 1'b1;
        assign signal_const = 1'b1;
        always @(negedge clock or negedge reset) begin
            if (reset == 0)
                signal_reg <= signal_const;
            else
                if (clear)
                    signal_reg <= vdd;
                else
                    if (enable)
                        signal_reg <= d;
        end
        assign q = signal_reg;

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
        signal signal_const : std_logic;
        signal signal_reg : std_logic;

    begin

        vdd <= '1';
        signal_const <= '1';
        process (clock, reset) begin
            if reset = '0' then
                signal_reg <= signal_const;
            else
                if falling_edge(clock) then
                    if clear = '1' then
                        signal_reg <= vdd;
                    else
                        if enable = '1' then
                            signal_reg <= d;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        q <= signal_reg;

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

        wire signal_const;
        reg c;
        wire b;
        wire a;
        assign signal_const = 1'b0;
        always @(posedge clock or posedge reset) begin
            if (reset)
                c <= signal_const;
            else
                if (clear)
                    c <= signal_const;
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

        signal signal_const : std_logic;
        signal c : std_logic;
        signal b : std_logic;
        signal a : std_logic;

    begin

        signal_const <= '0';
        process (clock, reset) begin
            if reset = '1' then
                c <= signal_const;
            else
                if rising_edge(clock) then
                    if clear = '1' then
                        c <= signal_const;
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
