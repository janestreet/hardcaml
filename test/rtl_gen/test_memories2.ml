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

        reg [31:0] signal_multiport_mem[0:127];
        wire [31:0] signal_mem_read_port;
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem[wa] <= d;
        end
        assign signal_mem_read_port = signal_multiport_mem[ra];
        assign q0 = signal_mem_read_port;

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

        type signal_multiport_mem_type is array (0 to 127) of std_logic_vector(31 downto 0);
        signal signal_multiport_mem : signal_multiport_mem_type;
        signal signal_mem_read_port : std_logic_vector(31 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem(to_integer(unsigned(wa))) <= d;
                end if;
            end if;
        end process;
        signal_mem_read_port <= signal_multiport_mem(to_integer(unsigned(ra)));
        q0 <= signal_mem_read_port;

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

        reg [14:0] signal_multiport_mem[0:1];
        wire [14:0] signal_mem_read_port;
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem[wa] <= d;
        end
        initial begin
            signal_multiport_mem[0] <= 15'b000000000000000;
            signal_multiport_mem[1] <= 15'b000000000000001;
        end
        assign signal_mem_read_port = signal_multiport_mem[ra];
        assign q0 = signal_mem_read_port;

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

        type signal_multiport_mem_type is array (0 to 1) of std_logic_vector(14 downto 0);
        signal signal_multiport_mem : signal_multiport_mem_type := (
            0 => "000000000000000",
            1 => "000000000000001"
        );
        signal signal_mem_read_port : std_logic_vector(14 downto 0);

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem(to_integer(unsigned(std_logic_vector'("" & wa)))) <= d;
                end if;
            end if;
        end process;
        signal_mem_read_port <= signal_multiport_mem(to_integer(unsigned(std_logic_vector'("" & ra))));
        q0 <= signal_mem_read_port;

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

        reg [0:0] signal_multiport_mem[0:3];
        wire signal_mem_read_port;
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem[wa] <= d;
        end
        initial begin
            signal_multiport_mem[0] <= 1'b0;
            signal_multiport_mem[1] <= 1'b1;
            signal_multiport_mem[2] <= 1'b0;
            signal_multiport_mem[3] <= 1'b1;
        end
        assign signal_mem_read_port = signal_multiport_mem[ra];
        assign q0 = signal_mem_read_port;

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

        type signal_multiport_mem_type is array (0 to 3) of std_logic;
        signal signal_multiport_mem : signal_multiport_mem_type := (
            0 => '0',
            1 => '1',
            2 => '0',
            3 => '1'
        );
        signal signal_mem_read_port : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem(to_integer(unsigned(wa))) <= d;
                end if;
            end if;
        end process;
        signal_mem_read_port <= signal_multiport_mem(to_integer(unsigned(ra)));
        q0 <= signal_mem_read_port;

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

        reg [0:0] signal_multiport_mem[0:1];
        wire signal_mem_read_port;
        always @(posedge clock) begin
            if (we)
                signal_multiport_mem[wa] <= d;
        end
        initial begin
            signal_multiport_mem[0] <= 1'b0;
            signal_multiport_mem[1] <= 1'b1;
        end
        assign signal_mem_read_port = signal_multiport_mem[ra];
        assign q0 = signal_mem_read_port;

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

        type signal_multiport_mem_type is array (0 to 1) of std_logic;
        signal signal_multiport_mem : signal_multiport_mem_type := (
            0 => '0',
            1 => '1'
        );
        signal signal_mem_read_port : std_logic;

    begin

        process (clock) begin
            if rising_edge(clock) then
                if we = '1' then
                    signal_multiport_mem(to_integer(unsigned(std_logic_vector'("" & wa)))) <= d;
                end if;
            end if;
        end process;
        signal_mem_read_port <= signal_multiport_mem(to_integer(unsigned(std_logic_vector'("" & ra))));
        q0 <= signal_mem_read_port;

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
      (Array.init memory_size ~f:(fun i -> Bits.of_int_trunc ~width:data_width i))
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

        wire [3:0] signal_const;
        wire [3:0] signal_add;
        wire [31:0] signal_mem_read_port;
        reg [31:0] signal_multiport_mem[0:15];
        wire [31:0] signal_mem_read_port_1;
        assign signal_const = 4'b0001;
        assign signal_add = read_address + signal_const;
        assign signal_mem_read_port = signal_multiport_mem[signal_add];
        initial begin
            signal_multiport_mem[0] <= 32'b00000000000000000000000000000000;
            signal_multiport_mem[1] <= 32'b00000000000000000000000000000001;
            signal_multiport_mem[2] <= 32'b00000000000000000000000000000010;
            signal_multiport_mem[3] <= 32'b00000000000000000000000000000011;
            signal_multiport_mem[4] <= 32'b00000000000000000000000000000100;
            signal_multiport_mem[5] <= 32'b00000000000000000000000000000101;
            signal_multiport_mem[6] <= 32'b00000000000000000000000000000110;
            signal_multiport_mem[7] <= 32'b00000000000000000000000000000111;
            signal_multiport_mem[8] <= 32'b00000000000000000000000000001000;
            signal_multiport_mem[9] <= 32'b00000000000000000000000000001001;
            signal_multiport_mem[10] <= 32'b00000000000000000000000000001010;
            signal_multiport_mem[11] <= 32'b00000000000000000000000000001011;
            signal_multiport_mem[12] <= 32'b00000000000000000000000000001100;
            signal_multiport_mem[13] <= 32'b00000000000000000000000000001101;
            signal_multiport_mem[14] <= 32'b00000000000000000000000000001110;
            signal_multiport_mem[15] <= 32'b00000000000000000000000000001111;
        end
        assign signal_mem_read_port_1 = signal_multiport_mem[read_address];
        assign q0 = signal_mem_read_port_1;
        assign q1 = signal_mem_read_port;

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

        signal signal_const : std_logic_vector(3 downto 0);
        signal signal_add : std_logic_vector(3 downto 0);
        signal signal_mem_read_port : std_logic_vector(31 downto 0);
        type signal_multiport_mem_type is array (0 to 15) of std_logic_vector(31 downto 0);
        signal signal_multiport_mem : signal_multiport_mem_type := (
            0 => "00000000000000000000000000000000",
            1 => "00000000000000000000000000000001",
            2 => "00000000000000000000000000000010",
            3 => "00000000000000000000000000000011",
            4 => "00000000000000000000000000000100",
            5 => "00000000000000000000000000000101",
            6 => "00000000000000000000000000000110",
            7 => "00000000000000000000000000000111",
            8 => "00000000000000000000000000001000",
            9 => "00000000000000000000000000001001",
            10 => "00000000000000000000000000001010",
            11 => "00000000000000000000000000001011",
            12 => "00000000000000000000000000001100",
            13 => "00000000000000000000000000001101",
            14 => "00000000000000000000000000001110",
            15 => "00000000000000000000000000001111"
        );
        signal signal_mem_read_port_1 : std_logic_vector(31 downto 0);

    begin

        signal_const <= "0001";
        signal_add <= std_logic_vector(unsigned(read_address) + unsigned(signal_const));
        signal_mem_read_port <= signal_multiport_mem(to_integer(unsigned(signal_add)));
        signal_mem_read_port_1 <= signal_multiport_mem(to_integer(unsigned(read_address)));
        q0 <= signal_mem_read_port_1;
        q1 <= signal_mem_read_port;

    end architecture;
    |}]
;;

let%expect_test "initialized memory remains writable" =
  let open Core in
  let circuit = circuit ~initialize:true ~data_width:8 ~address_width:1 () in
  let dir =
    Core_unix.mkdtemp (Filename.concat Filename.temp_dir_name "hardcaml-memory-")
  in
  Exn.protect
    ~f:(fun () ->
      Out_channel.write_all
        (Filename.concat dir "memory.vhd")
        ~data:(Testing.hdl false Vhdl circuit);
      Out_channel.write_all
        (Filename.concat dir "tb.vhd")
        ~data:
          {|
library ieee;
use ieee.std_logic_1164.all;
entity tb is end entity;
architecture test of tb is
    signal clock, we, wa, ra : std_logic := '0';
    signal d : std_logic_vector(7 downto 0) := (others => '0');
    signal q : std_logic_vector(7 downto 0);
begin
    dut: entity work.multimem port map(clock => clock, we => we, wa => wa,
                                      ra => ra, d => d, q0 => q);
    process begin
        wait for 1 ns;
        assert q = x"00" report "initial address 0" severity failure;
        ra <= '1'; wait for 1 ns;
        assert q = x"01" report "initial address 1" severity failure;
        we <= '1'; wa <= '0'; ra <= '0'; d <= x"ab";
        wait for 1 ns; clock <= '1'; wait for 1 ns;
        assert q = x"ab" report "write changed bits" severity failure;
        clock <= '0'; we <= '0'; d <= x"ff";
        wait for 1 ns; clock <= '1'; wait for 1 ns;
        assert q = x"ab" report "disabled write" severity failure;
        std.env.finish;
        wait;
    end process;
end architecture;
|};
      let ghdl = Hardcaml_tools_config.ghdl in
      let command =
        [%string
          "cd %{Filename.quote dir} && %{ghdl} -a --std=08 memory.vhd tb.vhd && \
           %{ghdl} --elab-run --std=08 tb --assert-level=error > /dev/null 2>&1"]
      in
      let result = Core_unix.system command in
      print_s [%sexp (result : Core_unix.Exit_or_signal.t)])
    ~finally:(fun () ->
      List.iter (Core_unix.ls_dir_detailed dir) ~f:(fun entry ->
        Core_unix.unlink (Filename.concat dir entry.name));
      Core_unix.rmdir dir);
  [%expect {| (Ok ()) |}]
;;
