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

        reg [7:0] _5;
        always @(posedge clock) begin
            if (enable)
                _5 <= d;
        end
        assign q = _5;

    endmodule
    |}];
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
            d : in std_logic_vector(7 downto 0);
            q : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of reg is

        signal \_5\ : std_logic_vector(7 downto 0);

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
    |}]
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

        wire [7:0] _8;
        reg [7:0] _9;
        assign _8 = 8'b00000000;
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
    |}];
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
            d : in std_logic_vector(7 downto 0);
            q : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of reg is

        signal \_8\ : std_logic_vector(7 downto 0);
        signal \_9\ : std_logic_vector(7 downto 0);

    begin

        \_8\ <= "00000000";
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
    |}]
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
        write_enable,
        write_data,
        write_address,
        clock,
        read_address,
        q
    );

        input write_enable;
        input [7:0] write_data;
        input [1:0] write_address;
        input clock;
        input [1:0] read_address;
        output [7:0] q;

        reg [7:0] _7[0:3];
        wire [7:0] _8;
        always @(posedge clock) begin
            if (write_enable)
                _7[write_address] <= write_data;
        end
        assign _8 = _7[read_address];
        assign q = _8;

    endmodule
    |}];
  Rtl.print Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity reg is
        port (
            write_enable : in std_logic;
            write_data : in std_logic_vector(7 downto 0);
            write_address : in std_logic_vector(1 downto 0);
            clock : in std_logic;
            read_address : in std_logic_vector(1 downto 0);
            q : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of reg is

        type \_7_type\ is array (0 to 3) of std_logic_vector(7 downto 0);
        signal \_7\ : \_7_type\;
        signal \_8\ : std_logic_vector(7 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if write_enable = '1' then
                    \_7\(to_integer(unsigned(write_address))) <= write_data;
                end if;
            end if;
        end process;
        \_8\ <= \_7\(to_integer(unsigned(read_address)));
        q <= \_8\;

    end architecture;
    |}]
;;

let%expect_test "multiport mem" =
  let q =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:4
      ~write_ports:[| { write_clock = clock; write_address; write_enable; write_data } |]
      ~read_ports:[| { read_clock = clock; read_address; read_enable } |]
      ()
  in
  let q = Array.to_list q |> List.mapi ~f:(fun i q -> output ("q" ^ Int.to_string i) q) in
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

        reg [7:0] _8[0:3];
        wire [7:0] _9;
        reg [7:0] _10;
        always @(posedge clock) begin
            if (write_enable)
                _8[write_address] <= write_data;
        end
        assign _9 = _8[read_address];
        always @(posedge clock) begin
            if (read_enable)
                _10 <= _9;
        end
        assign q0 = _10;

    endmodule
    |}];
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
            write_data : in std_logic_vector(7 downto 0);
            write_address : in std_logic_vector(1 downto 0);
            clock : in std_logic;
            read_address : in std_logic_vector(1 downto 0);
            q0 : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of reg is

        type \_8_type\ is array (0 to 3) of std_logic_vector(7 downto 0);
        signal \_8\ : \_8_type\;
        signal \_9\ : std_logic_vector(7 downto 0);
        signal \_10\ : std_logic_vector(7 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if write_enable = '1' then
                    \_8\(to_integer(unsigned(write_address))) <= write_data;
                end if;
            end if;
        end process;
        \_9\ <= \_8\(to_integer(unsigned(read_address)));
        process (clock) begin
            if rising_edge(clock) then
                if read_enable = '1' then
                    \_10\ <= \_9\;
                end if;
            end if;
        end process;
        q0 <= \_10\;

    end architecture;
    |}]
;;

let%expect_test "Try generate a Verilog circuit with a signal using a reserved name" =
  let spec = Reg_spec.create () ~clock in
  let reserved_word = "signed" in
  assert (List.mem Reserved_words.verilog reserved_word ~equal:String.equal);
  let q = reg spec ~enable d -- reserved_word in
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

        reg [7:0] signed_0;
        always @(posedge clock) begin
            if (enable)
                signed_0 <= d;
        end
        assign q = signed_0;

    endmodule
    |}]
;;

let%expect_test "Try to generate a Verilog module name with dashes" =
  let input = Signal.input "in" 32 in
  let a = wire 32 -- "a" in
  a <-- input;
  Expect_test_helpers_base.require_does_raise (fun () ->
    let circuit = Circuit.create_exn ~name:"mod-with-dash" [ output "out" a ] in
    Rtl.print Verilog circuit);
  [%expect
    {|
    ("Invalid module or instance name - should only contain alphanumeric or special characters"
     (name mod-with-dash)
     (special_chars (_ $)))
    |}]
;;

let%expect_test "Try to generate a Verilog module name that starts with a number" =
  let input = Signal.input "in" 32 in
  let a = wire 32 -- "a" in
  a <-- input;
  Expect_test_helpers_base.require_does_raise (fun () ->
    let circuit = Circuit.create_exn ~name:"999" [ output "out" a ] in
    Rtl.print Verilog circuit);
  [%expect
    {|
    ("First letter of module or instance names should be alpha or special"
     (name 999)
     (special_chars (_ $)))
    |}]
;;

let%expect_test "Module name rules apply to instantiations also" =
  Expect_test_helpers_base.require_does_raise (fun () ->
    let input = Signal.input "in" 32 in
    let outputs =
      Instantiation.create ~name:"a^b" ~inputs:[ "a", input ] ~outputs:[ "b", 1 ] ()
    in
    let circuit =
      Circuit.create_exn ~name:"foo" [ output "out" (Instantiation.output outputs "b") ]
    in
    Rtl.print Verilog circuit);
  [%expect
    {|
    ("Invalid module or instance name - should only contain alphanumeric or special characters"
     (name a^b)
     (special_chars (_ $)))
    |}]
;;

let%expect_test "Try to generate Verilog port names with dashes" =
  let input = input "in-with-dash" 32 in
  let a = wire 32 -- "a" in
  a <-- input;
  Expect_test_helpers_base.require_does_raise (fun () ->
    let circuit = Circuit.create_exn ~name:"mod" [ output "out-with-dash" a ] in
    Rtl.print Verilog circuit);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name mod)
      (hierarchy_path (mod))
      (exn (
        "[Illegal port name"
        (name       in-with-dash)
        (legal_name in_with_dash)
        (note       "Hardcaml will not change ports names.")
        (port ((wire (names (in-with-dash)) (width 32)))))))
    |}]
;;

let%expect_test "Try to generate Verilog net names with dashes" =
  let input = Signal.input "in" 32 in
  let a = wire 32 -- "a-with-dash" in
  a <-- input;
  let circuit = Circuit.create_exn ~name:"mod" [ output "out" a ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module mod (
        in,
        out
    );

        input [31:0] in;
        output [31:0] out;

        wire [31:0] a_with_dash;
        assign a_with_dash = in;
        assign out = a_with_dash;

    endmodule
    |}]
;;

let%expect_test "initial value of resisters" =
  let spec = Reg_spec.create () ~clock in
  let q = reg spec ~initialize_to:(Bits.of_string "00101011") ~enable d in
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

        reg [7:0] _5 = 8'b00101011;
        always @(posedge clock) begin
            if (enable)
                _5 <= d;
        end
        assign q = _5;

    endmodule
    |}];
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
            d : in std_logic_vector(7 downto 0);
            q : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of reg is

        signal \_5\ : std_logic_vector(7 downto 0) := "00101011";

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
    |}]
;;

let%expect_test "initial value of resisters with comment (only in Verilog)" =
  let spec = Reg_spec.create () ~clock in
  let q = reg spec ~initialize_to:(Bits.of_string "00101011") ~enable d in
  let q = set_comment q "some comment" in
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

        reg [7:0] _5/* some comment */ = 8'b00101011;
        always @(posedge clock) begin
            if (enable)
                _5 <= d;
        end
        assign q = _5;

    endmodule
    |}]
;;

let%expect_test "detects system verilog keyword" =
  let circuit =
    Circuit.create_exn ~name:"test" [ output "q" (wireof (input "d" 1) -- "virtual") ]
  in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module test (
        d,
        q
    );

        input d;
        output q;

        wire virtual;
        assign virtual = d;
        assign q = virtual;

    endmodule
    |}];
  Rtl.print Systemverilog circuit;
  [%expect
    {|
    module test (
        d,
        q
    );

        input d;
        output q;

        wire virtual_0;
        assign virtual_0 = d;
        assign q = virtual_0;

    endmodule
    |}]
;;
