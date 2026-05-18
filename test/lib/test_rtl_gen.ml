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

        reg [7:0] signal_reg;
        always @(posedge clock) begin
            if (enable)
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_reg : std_logic_vector(7 downto 0);

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

        wire [7:0] signal_const;
        reg [7:0] signal_reg;
        assign signal_const = 8'b00000000;
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

        signal signal_const : std_logic_vector(7 downto 0);
        signal signal_reg : std_logic_vector(7 downto 0);

    begin

        signal_const <= "00000000";
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

        reg [7:0] signal_multiport_mem[0:3];
        wire [7:0] signal_mem_read_port;
        always @(posedge clock) begin
            if (write_enable)
                signal_multiport_mem[write_address] <= write_data;
        end
        assign signal_mem_read_port = signal_multiport_mem[read_address];
        assign q = signal_mem_read_port;

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

        type signal_multiport_mem_type is array (0 to 3) of std_logic_vector(7 downto 0);
        signal signal_multiport_mem : signal_multiport_mem_type;
        signal signal_mem_read_port : std_logic_vector(7 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if write_enable = '1' then
                    signal_multiport_mem(to_integer(unsigned(write_address))) <= write_data;
                end if;
            end if;
        end process;
        signal_mem_read_port <= signal_multiport_mem(to_integer(unsigned(read_address)));
        q <= signal_mem_read_port;

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

        reg [7:0] signal_multiport_mem[0:3];
        wire [7:0] signal_mem_read_port;
        reg [7:0] signal_reg;
        always @(posedge clock) begin
            if (write_enable)
                signal_multiport_mem[write_address] <= write_data;
        end
        assign signal_mem_read_port = signal_multiport_mem[read_address];
        always @(posedge clock) begin
            if (read_enable)
                signal_reg <= signal_mem_read_port;
        end
        assign q0 = signal_reg;

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

        type signal_multiport_mem_type is array (0 to 3) of std_logic_vector(7 downto 0);
        signal signal_multiport_mem : signal_multiport_mem_type;
        signal signal_mem_read_port : std_logic_vector(7 downto 0);
        signal signal_reg : std_logic_vector(7 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if write_enable = '1' then
                    signal_multiport_mem(to_integer(unsigned(write_address))) <= write_data;
                end if;
            end if;
        end process;
        signal_mem_read_port <= signal_multiport_mem(to_integer(unsigned(read_address)));
        process (clock) begin
            if rising_edge(clock) then
                if read_enable = '1' then
                    signal_reg <= signal_mem_read_port;
                end if;
            end if;
        end process;
        q0 <= signal_reg;

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

        reg [7:0] \signed ;
        always @(posedge clock) begin
            if (enable)
                \signed  <= d;
        end
        assign q = \signed ;

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
  let circuit = Circuit.create_exn ~name:"mod" [ output "out-with-dash" a ] in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module mod (
        \in-with-dash ,
        \out-with-dash
    );

        input [31:0] \in-with-dash ;
        output [31:0] \out-with-dash ;

        wire [31:0] a;
        assign a = \in-with-dash ;
        assign \out-with-dash  = a;

    endmodule
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

        wire [31:0] \a-with-dash ;
        assign \a-with-dash  = in;
        assign out = \a-with-dash ;

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

        reg [7:0] signal_reg = 8'b00101011;
        always @(posedge clock) begin
            if (enable)
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        signal signal_reg : std_logic_vector(7 downto 0) := "00101011";

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

        reg [7:0] signal_reg/* some comment */ = 8'b00101011;
        always @(posedge clock) begin
            if (enable)
                signal_reg <= d;
        end
        assign q = signal_reg;

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

        wire \virtual ;
        assign \virtual  = d;
        assign q = \virtual ;

    endmodule
    |}];
  Rtl.print Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity test is
        port (
            d : in std_logic;
            q : out std_logic
        );
    end entity;

    architecture rtl of test is

        signal virtual : std_logic;

    begin

        virtual <= d;
        q <= virtual;

    end architecture;
    |}]
;;
