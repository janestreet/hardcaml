open! Import
open Hardcaml_waveterm_cyclesim

let write_port address_width data_width =
  { Write_port.write_clock = Signal.gnd
  ; write_address = Signal.of_int_trunc ~width:address_width 0
  ; write_data = Signal.of_int_trunc ~width:data_width 0
  ; write_enable = Signal.gnd
  }
;;

let%expect_test "exceptions" =
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 3 8 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:3 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] size does not match what can be addressed"
     (size          16)
     (address_width 3))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 5 8 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:3 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] size does not match what can be addressed"
     (size          16)
     (address_width 5))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:3 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of read address is inconsistent"
     (port               0)
     (read_address_width 3)
     (expected           4))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:5 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of read address is inconsistent"
     (port               0)
     (read_address_width 5)
     (expected           4))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:
        [| { (write_port 4 8) with write_clock = Signal.of_int_trunc ~width:2 0 } |]
      ~read_addresses:[| Signal.of_int_trunc ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of clock must be 1"
     (port               0)
     (write_enable_width 1))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:
        [| { (write_port 4 8) with write_enable = Signal.of_int_trunc ~width:2 0 } |]
      ~read_addresses:[| Signal.of_int_trunc ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of write enable must be 1"
     (port               0)
     (write_enable_width 2))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[||]
      ~read_addresses:[| Signal.of_int_trunc ~width:4 0 |]);
  [%expect {| "[Signal.multiport_memory] requires at least one write port" |}];
  require_does_raise (fun () ->
    Signal.multiport_memory 16 ~write_ports:[| write_port 4 8 |] ~read_addresses:[||]);
  [%expect {| "[Signal.multiport_memory] requires at least one read port" |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:4 0; Signal.of_int_trunc ~width:5 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of read address is inconsistent"
     (port               1)
     (read_address_width 5)
     (expected           4))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8; write_port 5 8 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of write address is inconsistent"
     (port                1)
     (write_address_width 5)
     (expected            4))
    |}];
  require_does_raise (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8; write_port 4 16 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of write data is inconsistent"
     (port             1)
     (write_data_width 16)
     (expected         8))
    |}]
;;

let%expect_test "sexp" =
  let sexp_of_signal = Signal.Type.sexp_of_signal_recursive ~depth:2 in
  let memory =
    Signal.multiport_memory
      32
      ~write_ports:[| write_port 5 12; write_port 5 12 |]
      ~read_addresses:[| Signal.of_int_trunc ~width:5 0; Signal.of_int_trunc ~width:5 0 |]
  in
  print_s [%message (memory : signal array)];
  [%expect
    {|
    (memory (
      (memory_read_port
        (width 12)
        ((memory (
           multiport_memory
           (width 12)
           ((size 32)
            (write_ports (
              ((write_enable 0b0) (write_address 0b00000) (write_data 0x000))
              ((write_enable 0b0) (write_address 0b00000) (write_data 0x000)))))))
         (read_addresses (
           const
           (width 5)
           (value 0b00000)))))
      (memory_read_port
        (width 12)
        ((memory (
           multiport_memory
           (width 12)
           ((size 32)
            (write_ports (
              ((write_enable 0b0) (write_address 0b00000) (write_data 0x000))
              ((write_enable 0b0) (write_address 0b00000) (write_data 0x000)))))))
         (read_addresses (
           const
           (width 5)
           (value 0b00000)))))))
    |}]
;;

let%expect_test "verilog, async memory, 1 port" =
  let read_data =
    Signal.multiport_memory
      32
      ~write_ports:
        [| { write_clock = clock
           ; write_enable = Signal.input "write_enable" 1
           ; write_address = Signal.input "write_address" 5
           ; write_data = Signal.input "write_data" 15
           }
        |]
      ~read_addresses:[| Signal.input "read_address" 5 |]
  in
  let circuit =
    Circuit.create_exn
      ~name:"single_port_memory"
      (List.mapi (Array.to_list read_data) ~f:(fun i q ->
         Signal.output ("q" ^ Int.to_string i) q))
  in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module single_port_memory (
        write_enable,
        write_data,
        write_address,
        clock,
        read_address,
        q0
    );

        input write_enable;
        input [14:0] write_data;
        input [4:0] write_address;
        input clock;
        input [4:0] read_address;
        output [14:0] q0;

        reg [14:0] _7[0:31];
        wire [14:0] _8;
        always @(posedge clock) begin
            if (write_enable)
                _7[write_address] <= write_data;
        end
        assign _8 = _7[read_address];
        assign q0 = _8;

    endmodule
    |}]
;;

let%expect_test "verilog, async memory, 2 ports" =
  let read_data =
    Signal.multiport_memory
      32
      ~write_ports:
        [| { write_clock = clock
           ; write_enable = Signal.input "write_enable" 1
           ; write_address = Signal.input "write_address" 5
           ; write_data = Signal.input "write_data" 15
           }
         ; { write_clock = Signal.input "clock2" 1
           ; write_enable = Signal.input "write_enable2" 1
           ; write_address = Signal.input "write_address2" 5
           ; write_data = Signal.input "write_data2" 15
           }
        |]
      ~read_addresses:[| Signal.input "read_address" 5; Signal.input "read_address2" 5 |]
  in
  let circuit =
    Circuit.create_exn
      ~name:"multi_port_memory"
      (List.mapi (Array.to_list read_data) ~f:(fun i q ->
         Signal.output ("q" ^ Int.to_string i) q))
  in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module multi_port_memory (
        read_address2,
        write_enable2,
        write_data2,
        write_address2,
        clock2,
        write_enable,
        write_data,
        write_address,
        clock,
        read_address,
        q0,
        q1
    );

        input [4:0] read_address2;
        input write_enable2;
        input [14:0] write_data2;
        input [4:0] write_address2;
        input clock2;
        input write_enable;
        input [14:0] write_data;
        input [4:0] write_address;
        input clock;
        input [4:0] read_address;
        output [14:0] q0;
        output [14:0] q1;

        wire [14:0] _14;
        reg [14:0] _13[0:31];
        wire [14:0] _15;
        assign _14 = _13[read_address2];
        always @(posedge clock) begin
            if (write_enable)
                _13[write_address] <= write_data;
        end
        always @(posedge clock2) begin
            if (write_enable2)
                _13[write_address2] <= write_data2;
        end
        assign _15 = _13[read_address];
        assign q0 = _15;
        assign q1 = _14;

    endmodule
    |}]
;;

let dual_port ?(collision_mode = Ram.Collision_mode.Read_before_write) () =
  let read_data =
    Ram.create
      ~name:"foo"
      ~collision_mode
      ~size:32
      ~write_ports:
        [| { write_clock = Signal.input "write_clock1" 1
           ; write_enable = Signal.input "write_enable1" 1
           ; write_address = Signal.input "write_address1" 5
           ; write_data = Signal.input "write_data1" 15
           }
         ; { write_clock = Signal.input "write_clock2" 1
           ; write_enable = Signal.input "write_enable2" 1
           ; write_address = Signal.input "write_address2" 5
           ; write_data = Signal.input "write_data2" 15
           }
        |]
      ~read_ports:
        [| { Read_port.read_clock = Signal.input "read_clock1" 1
           ; read_address = Signal.input "read_address1" 5
           ; read_enable = Signal.input "read_enable1" 1
           }
         ; { read_clock = Signal.input "read_clock2" 1
           ; read_address = Signal.input "read_address2" 5
           ; read_enable = Signal.input "read_enable2" 1
           }
        |]
      ()
  in
  Circuit.create_exn
    ~name:"multi_port_memory"
    (List.mapi (Array.to_list read_data) ~f:(fun i q ->
       Signal.output ("q" ^ Int.to_string i) q))
;;

let%expect_test "dual port Verilog" =
  let circuit = dual_port () in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module multi_port_memory (
        read_enable2,
        read_clock2,
        read_address2,
        read_enable1,
        read_clock1,
        write_enable2,
        write_data2,
        write_address2,
        write_clock2,
        write_enable1,
        write_data1,
        write_address1,
        write_clock1,
        read_address1,
        q0,
        q1
    );

        input read_enable2;
        input read_clock2;
        input [4:0] read_address2;
        input read_enable1;
        input read_clock1;
        input write_enable2;
        input [14:0] write_data2;
        input [4:0] write_address2;
        input write_clock2;
        input write_enable1;
        input [14:0] write_data1;
        input [4:0] write_address1;
        input write_clock1;
        input [4:0] read_address1;
        output [14:0] q0;
        output [14:0] q1;

        wire [14:0] _18;
        reg [14:0] _19;
        reg [14:0] foo[0:31];
        wire [14:0] _20;
        reg [14:0] _21;
        assign _18 = foo[read_address2];
        always @(posedge read_clock2) begin
            if (read_enable2)
                _19 <= _18;
        end
        always @(posedge write_clock1) begin
            if (write_enable1)
                foo[write_address1] <= write_data1;
        end
        always @(posedge write_clock2) begin
            if (write_enable2)
                foo[write_address2] <= write_data2;
        end
        assign _20 = foo[read_address1];
        always @(posedge read_clock1) begin
            if (read_enable1)
                _21 <= _20;
        end
        assign q0 = _21;
        assign q1 = _19;

    endmodule
    |}]
;;

let%expect_test "dual port VHDL" =
  let circuit = dual_port () in
  Rtl.print Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity multi_port_memory is
        port (
            read_enable2 : in std_logic;
            read_clock2 : in std_logic;
            read_address2 : in std_logic_vector(4 downto 0);
            read_enable1 : in std_logic;
            read_clock1 : in std_logic;
            write_enable2 : in std_logic;
            write_data2 : in std_logic_vector(14 downto 0);
            write_address2 : in std_logic_vector(4 downto 0);
            write_clock2 : in std_logic;
            write_enable1 : in std_logic;
            write_data1 : in std_logic_vector(14 downto 0);
            write_address1 : in std_logic_vector(4 downto 0);
            write_clock1 : in std_logic;
            read_address1 : in std_logic_vector(4 downto 0);
            q0 : out std_logic_vector(14 downto 0);
            q1 : out std_logic_vector(14 downto 0)
        );
    end entity;

    architecture rtl of multi_port_memory is

        signal \_18\ : std_logic_vector(14 downto 0);
        signal \_19\ : std_logic_vector(14 downto 0);
        type foo_type is array (0 to 31) of std_logic_vector(14 downto 0);
        signal foo : foo_type;
        signal \_20\ : std_logic_vector(14 downto 0);
        signal \_21\ : std_logic_vector(14 downto 0);

    begin

        \_18\ <= foo(to_integer(unsigned(read_address2)));
        process (read_clock2) begin
            if rising_edge(read_clock2) then
                if read_enable2 = '1' then
                    \_19\ <= \_18\;
                end if;
            end if;
        end process;
        process (write_clock1, write_clock2) begin
            if rising_edge(write_clock1) then
                if write_enable1 = '1' then
                    foo(to_integer(unsigned(write_address1))) <= write_data1;
                end if;
            end if;
            if rising_edge(write_clock2) then
                if write_enable2 = '1' then
                    foo(to_integer(unsigned(write_address2))) <= write_data2;
                end if;
            end if;
        end process;
        \_20\ <= foo(to_integer(unsigned(read_address1)));
        process (read_clock1) begin
            if rising_edge(read_clock1) then
                if read_enable1 = '1' then
                    \_21\ <= \_20\;
                end if;
            end if;
        end process;
        q0 <= \_21\;
        q1 <= \_19\;

    end architecture;
    |}];
  Rtl.print ~config:{ Rtl.Config.default with two_state = true } Vhdl circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_bit.all;

    entity multi_port_memory is
        port (
            read_enable2 : in bit;
            read_clock2 : in bit;
            read_address2 : in bit_vector(4 downto 0);
            read_enable1 : in bit;
            read_clock1 : in bit;
            write_enable2 : in bit;
            write_data2 : in bit_vector(14 downto 0);
            write_address2 : in bit_vector(4 downto 0);
            write_clock2 : in bit;
            write_enable1 : in bit;
            write_data1 : in bit_vector(14 downto 0);
            write_address1 : in bit_vector(4 downto 0);
            write_clock1 : in bit;
            read_address1 : in bit_vector(4 downto 0);
            q0 : out bit_vector(14 downto 0);
            q1 : out bit_vector(14 downto 0)
        );
    end entity;

    architecture rtl of multi_port_memory is
        -- Conversions
        function to_stdlogic(i : in bit) return std_logic is
        begin
            if i = '0' then
                return '0';
            else
                return '1';
            end if;
        end function;

        signal \_18\ : bit_vector(14 downto 0);
        signal \_19\ : bit_vector(14 downto 0);
        type foo_type is array (0 to 31) of std_logic_vector(14 downto 0);
        signal foo : foo_type;
        signal \_20\ : bit_vector(14 downto 0);
        signal \_21\ : bit_vector(14 downto 0);

    begin

        \_18\ <= to_bitvector(foo(to_integer(unsigned(read_address2))));
        process (read_clock2) begin
            if rising_edge(read_clock2) then
                if read_enable2 = '1' then
                    \_19\ <= \_18\;
                end if;
            end if;
        end process;
        process (write_clock1, write_clock2) begin
            if rising_edge(write_clock1) then
                if write_enable1 = '1' then
                    foo(to_integer(unsigned(write_address1))) <= to_stdlogicvector(write_data1);
                end if;
            end if;
            if rising_edge(write_clock2) then
                if write_enable2 = '1' then
                    foo(to_integer(unsigned(write_address2))) <= to_stdlogicvector(write_data2);
                end if;
            end if;
        end process;
        \_20\ <= to_bitvector(foo(to_integer(unsigned(read_address1))));
        process (read_clock1) begin
            if rising_edge(read_clock1) then
                if read_enable1 = '1' then
                    \_21\ <= \_20\;
                end if;
            end if;
        end process;
        q0 <= \_21\;
        q1 <= \_19\;

    end architecture;
    |}]
;;

let%expect_test "simulation - write and read data on both ports" =
  let circuit = dual_port () in
  let simulator = Cyclesim.create circuit in
  let waves, simulator = Waveform.create simulator in
  let write_enable1 = Cyclesim.in_port simulator "write_enable1" in
  let write_address1 = Cyclesim.in_port simulator "write_address1" in
  let write_data1 = Cyclesim.in_port simulator "write_data1" in
  let _write_enable2 = Cyclesim.in_port simulator "write_enable2" in
  let _write_address2 = Cyclesim.in_port simulator "write_address2" in
  let _write_data2 = Cyclesim.in_port simulator "write_data2" in
  let read_address1 = Cyclesim.in_port simulator "read_address1" in
  let read_enable1 = Cyclesim.in_port simulator "read_enable1" in
  let read_address2 = Cyclesim.in_port simulator "read_address2" in
  let read_enable2 = Cyclesim.in_port simulator "read_enable2" in
  Cyclesim.reset simulator;
  (* write on port 1 and 2 *)
  write_enable1 := Bits.vdd;
  write_address1 := Bits.of_int_trunc ~width:5 3;
  write_data1 := Bits.of_int_trunc ~width:15 100;
  Cyclesim.cycle simulator;
  write_address1 := Bits.of_int_trunc ~width:5 4;
  write_data1 := Bits.of_int_trunc ~width:15 640;
  Cyclesim.cycle simulator;
  write_enable1 := Bits.gnd;
  (* read on port 1 *)
  Cyclesim.cycle simulator;
  read_address1 := Bits.of_int_trunc ~width:5 3;
  read_enable1 := Bits.vdd;
  Cyclesim.cycle simulator;
  (* read on port 2 *)
  read_enable1 := Bits.gnd;
  read_address2 := Bits.of_int_trunc ~width:5 3;
  read_enable2 := Bits.vdd;
  Cyclesim.cycle simulator;
  read_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  (* read on ports 1 and 2 *)
  read_enable1 := Bits.vdd;
  read_enable2 := Bits.vdd;
  read_address1 := Bits.of_int_trunc ~width:5 4;
  read_address2 := Bits.of_int_trunc ~width:5 4;
  Cyclesim.cycle simulator;
  read_enable1 := Bits.gnd;
  read_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  Waveform.print ~display_width:86 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────┐
    │                  ││────────────────────────┬─────────────────┬───────────          │
    │read_address1     ││ 00                     │03               │04                   │
    │                  ││────────────────────────┴─────────────────┴───────────          │
    │                  ││──────────────────────────────┬───────────┬───────────          │
    │read_address2     ││ 00                           │03         │04                   │
    │                  ││──────────────────────────────┴───────────┴───────────          │
    │read_clock1       ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │read_clock2       ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │read_enable1      ││                        ┌─────┐           ┌─────┐               │
    │                  ││────────────────────────┘     └───────────┘     └─────          │
    │read_enable2      ││                              ┌─────┐     ┌─────┐               │
    │                  ││──────────────────────────────┘     └─────┘     └─────          │
    │                  ││──────┬─────┬─────────────────────────────────────────          │
    │write_address1    ││ 00   │03   │04                                                 │
    │                  ││──────┴─────┴─────────────────────────────────────────          │
    │                  ││──────────────────────────────────────────────────────          │
    │write_address2    ││ 00                                                             │
    │                  ││──────────────────────────────────────────────────────          │
    │write_clock1      ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │write_clock2      ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │                  ││──────┬─────┬─────────────────────────────────────────          │
    │write_data1       ││ 0000 │0064 │0280                                               │
    │                  ││──────┴─────┴─────────────────────────────────────────          │
    │                  ││──────────────────────────────────────────────────────          │
    │write_data2       ││ 0000                                                           │
    │                  ││──────────────────────────────────────────────────────          │
    │write_enable1     ││      ┌───────────┐                                             │
    │                  ││──────┘           └───────────────────────────────────          │
    │write_enable2     ││                                                                │
    │                  ││──────────────────────────────────────────────────────          │
    │                  ││──────────────────────────────┬─────────────────┬─────          │
    │q0                ││ 0000                         │0064             │0280           │
    │                  ││──────────────────────────────┴─────────────────┴─────          │
    │                  ││────────────────────────────────────┬───────────┬─────          │
    │q1                ││ 0000                               │0064       │0280           │
    │                  ││────────────────────────────────────┴───────────┴─────          │
    └──────────────────┘└────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "simulation - write on both ports - highest indexed port wins" =
  let circuit = dual_port () in
  let simulator = Cyclesim.create circuit in
  let waves, simulator = Waveform.create simulator in
  let write_enable1 = Cyclesim.in_port simulator "write_enable1" in
  let write_address1 = Cyclesim.in_port simulator "write_address1" in
  let write_data1 = Cyclesim.in_port simulator "write_data1" in
  let write_enable2 = Cyclesim.in_port simulator "write_enable2" in
  let write_address2 = Cyclesim.in_port simulator "write_address2" in
  let write_data2 = Cyclesim.in_port simulator "write_data2" in
  let read_address1 = Cyclesim.in_port simulator "read_address1" in
  let read_enable1 = Cyclesim.in_port simulator "read_enable1" in
  let read_address2 = Cyclesim.in_port simulator "read_address2" in
  let read_enable2 = Cyclesim.in_port simulator "read_enable2" in
  Cyclesim.reset simulator;
  write_enable1 := Bits.vdd;
  write_address1 := Bits.of_int_trunc ~width:5 9;
  write_data1 := Bits.of_int_trunc ~width:15 100;
  write_enable2 := Bits.vdd;
  write_address2 := Bits.of_int_trunc ~width:5 9;
  write_data2 := Bits.of_int_trunc ~width:15 200;
  Cyclesim.cycle simulator;
  write_enable1 := Bits.gnd;
  write_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  read_enable1 := Bits.vdd;
  read_address1 := Bits.of_int_trunc ~width:5 9;
  read_enable2 := Bits.vdd;
  read_address2 := Bits.of_int_trunc ~width:5 9;
  Cyclesim.cycle simulator;
  read_enable1 := Bits.gnd;
  read_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  Waveform.print ~display_width:60 ~wave_width:2 waves;
  [%expect
    {|
    ┌Signals──────┐┌Waves──────────────────────────────────────┐
    │             ││──────────────────┬───────────             │
    │read_address1││ 00               │09                      │
    │             ││──────────────────┴───────────             │
    │             ││──────────────────┬───────────             │
    │read_address2││ 00               │09                      │
    │             ││──────────────────┴───────────             │
    │read_clock1  ││                                           │
    │             ││──────────────────────────────             │
    │read_clock2  ││                                           │
    │             ││──────────────────────────────             │
    │read_enable1 ││                  ┌─────┐                  │
    │             ││──────────────────┘     └─────             │
    │read_enable2 ││                  ┌─────┐                  │
    │             ││──────────────────┘     └─────             │
    │             ││──────┬───────────────────────             │
    │write_address││ 00   │09                                  │
    │             ││──────┴───────────────────────             │
    │             ││──────┬───────────────────────             │
    │write_address││ 00   │09                                  │
    │             ││──────┴───────────────────────             │
    │write_clock1 ││                                           │
    │             ││──────────────────────────────             │
    │write_clock2 ││                                           │
    │             ││──────────────────────────────             │
    │             ││──────┬───────────────────────             │
    │write_data1  ││ 0000 │0064                                │
    │             ││──────┴───────────────────────             │
    │             ││──────┬───────────────────────             │
    │write_data2  ││ 0000 │00C8                                │
    │             ││──────┴───────────────────────             │
    │write_enable1││      ┌─────┐                              │
    │             ││──────┘     └─────────────────             │
    │write_enable2││      ┌─────┐                              │
    │             ││──────┘     └─────────────────             │
    │             ││────────────────────────┬─────             │
    │q0           ││ 0000                   │00C8              │
    │             ││────────────────────────┴─────             │
    │             ││────────────────────────┬─────             │
    │q1           ││ 0000                   │00C8              │
    │             ││────────────────────────┴─────             │
    └─────────────┘└───────────────────────────────────────────┘
    |}]
;;

let%expect_test "simulation - demonstrate collision modes" =
  let test collision_mode =
    let circuit = dual_port ~collision_mode () in
    let simulator = Cyclesim.create circuit in
    let waves, simulator = Waveform.create simulator in
    let write_enable1 = Cyclesim.in_port simulator "write_enable1" in
    let write_address1 = Cyclesim.in_port simulator "write_address1" in
    let write_data1 = Cyclesim.in_port simulator "write_data1" in
    let read_address1 = Cyclesim.in_port simulator "read_address1" in
    let read_enable1 = Cyclesim.in_port simulator "read_enable1" in
    Cyclesim.reset simulator;
    write_enable1 := Bits.vdd;
    write_address1 := Bits.of_int_trunc ~width:5 13;
    write_data1 := Bits.of_int_trunc ~width:15 10;
    Cyclesim.cycle simulator;
    write_enable1 := Bits.vdd;
    write_address1 := Bits.of_int_trunc ~width:5 13;
    write_data1 := Bits.of_int_trunc ~width:15 20;
    read_enable1 := Bits.vdd;
    read_address1 := Bits.of_int_trunc ~width:5 13;
    Cyclesim.cycle simulator;
    write_enable1 := Bits.gnd;
    read_enable1 := Bits.gnd;
    Cyclesim.cycle simulator;
    Waveform.print ~display_width:60 ~wave_width:2 waves
  in
  test Read_before_write;
  [%expect
    {|
    ┌Signals──────┐┌Waves──────────────────────────────────────┐
    │             ││────────────┬───────────                   │
    │read_address1││ 00         │0D                            │
    │             ││────────────┴───────────                   │
    │             ││────────────────────────                   │
    │read_address2││ 00                                        │
    │             ││────────────────────────                   │
    │read_clock1  ││                                           │
    │             ││────────────────────────                   │
    │read_clock2  ││                                           │
    │             ││────────────────────────                   │
    │read_enable1 ││            ┌─────┐                        │
    │             ││────────────┘     └─────                   │
    │read_enable2 ││                                           │
    │             ││────────────────────────                   │
    │             ││──────┬─────────────────                   │
    │write_address││ 00   │0D                                  │
    │             ││──────┴─────────────────                   │
    │             ││────────────────────────                   │
    │write_address││ 00                                        │
    │             ││────────────────────────                   │
    │write_clock1 ││                                           │
    │             ││────────────────────────                   │
    │write_clock2 ││                                           │
    │             ││────────────────────────                   │
    │             ││──────┬─────┬───────────                   │
    │write_data1  ││ 0000 │000A │0014                          │
    │             ││──────┴─────┴───────────                   │
    │             ││────────────────────────                   │
    │write_data2  ││ 0000                                      │
    │             ││────────────────────────                   │
    │write_enable1││      ┌───────────┐                        │
    │             ││──────┘           └─────                   │
    │write_enable2││                                           │
    │             ││────────────────────────                   │
    │             ││──────────────────┬─────                   │
    │q0           ││ 0000             │000A                    │
    │             ││──────────────────┴─────                   │
    │             ││────────────────────────                   │
    │q1           ││ 0000                                      │
    │             ││────────────────────────                   │
    └─────────────┘└───────────────────────────────────────────┘
    |}];
  test Write_before_read;
  [%expect
    {|
    ┌Signals──────┐┌Waves──────────────────────────────────────┐
    │             ││────────────┬───────────                   │
    │read_address1││ 00         │0D                            │
    │             ││────────────┴───────────                   │
    │             ││────────────────────────                   │
    │read_address2││ 00                                        │
    │             ││────────────────────────                   │
    │read_clock1  ││                                           │
    │             ││────────────────────────                   │
    │read_clock2  ││                                           │
    │             ││────────────────────────                   │
    │read_enable1 ││            ┌─────┐                        │
    │             ││────────────┘     └─────                   │
    │read_enable2 ││                                           │
    │             ││────────────────────────                   │
    │             ││──────┬─────────────────                   │
    │write_address││ 00   │0D                                  │
    │             ││──────┴─────────────────                   │
    │             ││────────────────────────                   │
    │write_address││ 00                                        │
    │             ││────────────────────────                   │
    │write_clock1 ││                                           │
    │             ││────────────────────────                   │
    │write_clock2 ││                                           │
    │             ││────────────────────────                   │
    │             ││──────┬─────┬───────────                   │
    │write_data1  ││ 0000 │000A │0014                          │
    │             ││──────┴─────┴───────────                   │
    │             ││────────────────────────                   │
    │write_data2  ││ 0000                                      │
    │             ││────────────────────────                   │
    │write_enable1││      ┌───────────┐                        │
    │             ││──────┘           └─────                   │
    │write_enable2││                                           │
    │             ││────────────────────────                   │
    │             ││──────────────────┬─────                   │
    │q0           ││ 0000             │0014                    │
    │             ││──────────────────┴─────                   │
    │             ││────────────────────────                   │
    │q1           ││ 0000                                      │
    │             ││────────────────────────                   │
    └─────────────┘└───────────────────────────────────────────┘
    |}]
;;

let%expect_test "memory initialization" =
  let test address_width data_width =
    let memory_size = 1 lsl address_width in
    let read_data =
      Signal.multiport_memory
        memory_size
        ~write_ports:[| write_port address_width data_width |]
        ~read_addresses:[| Signal.input "read_address" address_width |]
        ~initialize_to:
          (Array.init memory_size ~f:(fun i -> Bits.of_int_trunc ~width:data_width i))
    in
    let circuit =
      Circuit.create_exn
        ~name:"initialized_memory"
        (List.mapi (Array.to_list read_data) ~f:(fun i q ->
           Signal.output ("q" ^ Int.to_string i) q))
    in
    Rtl.print Verilog circuit;
    Rtl.print Vhdl circuit
  in
  test 2 8;
  [%expect
    {|
    module initialized_memory (
        read_address,
        q0
    );

        input [1:0] read_address;
        output [7:0] q0;

        wire [7:0] _5;
        wire [1:0] _4;
        wire gnd;
        reg [7:0] _6[0:3];
        wire [7:0] _7;
        assign _5 = 8'b00000000;
        assign _4 = 2'b00;
        assign gnd = 1'b0;
        always @(posedge gnd) begin
            if (gnd)
                _6[_4] <= _5;
        end
        initial begin
            _6[0] <= 8'b00000000;
            _6[1] <= 8'b00000001;
            _6[2] <= 8'b00000010;
            _6[3] <= 8'b00000011;
        end
        assign _7 = _6[read_address];
        assign q0 = _7;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity initialized_memory is
        port (
            read_address : in std_logic_vector(1 downto 0);
            q0 : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of initialized_memory is

        signal \_5\ : std_logic_vector(7 downto 0);
        signal \_4\ : std_logic_vector(1 downto 0);
        signal gnd : std_logic;
        type \_6_type\ is array (0 to 3) of std_logic_vector(7 downto 0);
        signal \_6\ : \_6_type\;
        signal \_7\ : std_logic_vector(7 downto 0);

    begin

        \_5\ <= "00000000";
        \_4\ <= "00";
        gnd <= '0';
        process (gnd) begin
            if rising_edge(gnd) then
                if gnd = '1' then
                    \_6\(to_integer(unsigned(\_4\))) <= \_5\;
                end if;
            end if;
        end process;
        process begin
            \_6\(0) <= "00000000";
            \_6\(1) <= "00000001";
            \_6\(2) <= "00000010";
            \_6\(3) <= "00000011";
            wait;
        end process;
        \_7\ <= \_6\(to_integer(unsigned(read_address)));
        q0 <= \_7\;

    end architecture;
    |}];
  test 3 1;
  [%expect
    {|
    module initialized_memory (
        read_address,
        q0
    );

        input [2:0] read_address;
        output q0;

        wire _5;
        wire [2:0] _4;
        wire gnd;
        reg [0:0] _6[0:7];
        wire _7;
        assign _5 = 1'b0;
        assign _4 = 3'b000;
        assign gnd = 1'b0;
        always @(posedge gnd) begin
            if (gnd)
                _6[_4] <= _5;
        end
        initial begin
            _6[0] <= 1'b0;
            _6[1] <= 1'b1;
            _6[2] <= 1'b0;
            _6[3] <= 1'b1;
            _6[4] <= 1'b0;
            _6[5] <= 1'b1;
            _6[6] <= 1'b0;
            _6[7] <= 1'b1;
        end
        assign _7 = _6[read_address];
        assign q0 = _7;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity initialized_memory is
        port (
            read_address : in std_logic_vector(2 downto 0);
            q0 : out std_logic
        );
    end entity;

    architecture rtl of initialized_memory is

        signal \_5\ : std_logic;
        signal \_4\ : std_logic_vector(2 downto 0);
        signal gnd : std_logic;
        type \_6_type\ is array (0 to 7) of std_logic;
        signal \_6\ : \_6_type\;
        signal \_7\ : std_logic;

    begin

        \_5\ <= '0';
        \_4\ <= "000";
        gnd <= '0';
        process (gnd) begin
            if rising_edge(gnd) then
                if gnd = '1' then
                    \_6\(to_integer(unsigned(\_4\))) <= \_5\;
                end if;
            end if;
        end process;
        process begin
            \_6\(0) <= '0';
            \_6\(1) <= '1';
            \_6\(2) <= '0';
            \_6\(3) <= '1';
            \_6\(4) <= '0';
            \_6\(5) <= '1';
            \_6\(6) <= '0';
            \_6\(7) <= '1';
            wait;
        end process;
        \_7\ <= \_6\(to_integer(unsigned(read_address)));
        q0 <= \_7\;

    end architecture;
    |}]
;;

let%expect_test "initialized memory" =
  let address_width = 3 in
  let data_width = 8 in
  let memory_size = 1 lsl address_width in
  let read_address = Signal.input "read_address" address_width in
  let read_data =
    Signal.multiport_memory
      memory_size
      ~write_ports:
        [| Write_port.(
             map2
               port_names
               { write_clock = 1
               ; write_data = data_width
               ; write_enable = 1
               ; write_address = address_width
               }
               ~f:Signal.input)
        |]
      ~read_addresses:[| read_address; Signal.( +:. ) read_address 1 |]
      ~initialize_to:
        (Array.init memory_size ~f:(fun i -> Bits.of_int_trunc ~width:data_width (i + 10)))
  in
  let circuit =
    Circuit.create_exn
      ~name:"rom"
      (List.mapi (Array.to_list read_data) ~f:(fun i q ->
         Signal.output ("q" ^ Int.to_string i) q))
  in
  let sim = Cyclesim.create circuit in
  let waves, sim = Waveform.create sim in
  let read_address = Cyclesim.in_port sim "read_address" in
  for i = 0 to memory_size - 1 do
    read_address := Bits.of_int_trunc ~width:address_width i;
    Cyclesim.cycle sim
  done;
  read_address := Bits.of_int_trunc ~width:address_width 4;
  Cyclesim.in_port sim "write_address" := Bits.of_int_trunc ~width:address_width 4;
  Cyclesim.in_port sim "write_enable" := Bits.vdd;
  Cyclesim.in_port sim "write_data" := Bits.of_int_trunc ~width:data_width 255;
  Cyclesim.cycle sim;
  Cyclesim.in_port sim "write_address" := Bits.of_int_trunc ~width:address_width 5;
  Cyclesim.in_port sim "write_enable" := Bits.vdd;
  Cyclesim.in_port sim "write_data" := Bits.of_int_trunc ~width:data_width 254;
  Cyclesim.cycle sim;
  Cyclesim.in_port sim "write_enable" := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  Waveform.print ~display_width:88 ~wave_width:1 waves;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───────────────                  │
    │read_address      ││ 0  │1  │2  │3  │4  │5  │6  │7  │4                                │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───────────────                  │
    │                  ││────────────────────────────────┬───┬───────────                  │
    │write_address     ││ 0                              │4  │5                            │
    │                  ││────────────────────────────────┴───┴───────────                  │
    │write_clock       ││                                                                  │
    │                  ││────────────────────────────────────────────────                  │
    │                  ││────────────────────────────────┬───┬───────────                  │
    │write_data        ││ 00                             │FF │FE                           │
    │                  ││────────────────────────────────┴───┴───────────                  │
    │write_enable      ││                                ┌───────┐                         │
    │                  ││────────────────────────────────┘       └───────                  │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───────────                  │
    │q0                ││ 0A │0B │0C │0D │0E │0F │10 │11 │0E │FF                           │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───────────                  │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───────┬───────                  │
    │q1                ││ 0B │0C │0D │0E │0F │10 │11 │0A │0F     │FE                       │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───────┴───────                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "rom" =
  let test address_width data_width =
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
    let sim = Cyclesim.create circuit in
    let waves, sim = Waveform.create sim in
    let read_address = Cyclesim.in_port sim "read_address" in
    for i = 0 to memory_size - 1 do
      read_address := Bits.of_int_trunc ~width:address_width i;
      Cyclesim.cycle sim
    done;
    Waveform.print ~display_width:88 ~wave_width:1 waves
  in
  test 2 4;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───┬───                                                  │
    │read_address      ││ 0  │1  │2  │3                                                    │
    │                  ││────┴───┴───┴───                                                  │
    │                  ││────┬───┬───┬───                                                  │
    │q0                ││ 0  │1  │2  │3                                                    │
    │                  ││────┴───┴───┴───                                                  │
    │                  ││────┬───┬───┬───                                                  │
    │q1                ││ 1  │2  │3  │0                                                    │
    │                  ││────┴───┴───┴───                                                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    |}];
  test 4 8;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───  │
    │read_address      ││ 0  │1  │2  │3  │4  │5  │6  │7  │8  │9  │A  │B  │C  │D  │E  │F    │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───  │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───  │
    │q0                ││ 00 │01 │02 │03 │04 │05 │06 │07 │08 │09 │0A │0B │0C │0D │0E │0F   │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───  │
    │                  ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───  │
    │q1                ││ 01 │02 │03 │04 │05 │06 │07 │08 │09 │0A │0B │0C │0D │0E │0F │00   │
    │                  ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────┘
    |}]
;;
