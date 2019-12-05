open! Import

let write_port address_width data_width =
  { Signal.write_clock = Signal.gnd
  ; write_address = Signal.of_int ~width:address_width 0
  ; write_data = Signal.of_int ~width:data_width 0
  ; write_enable = Signal.gnd
  }
;;

let%expect_test "exceptions" =
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 3 8 |]
      ~read_addresses:[| Signal.of_int ~width:3 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] size is greater than what can be addressed by write port"
     (size          16)
     (address_width 3)) |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8 |]
      ~read_addresses:[| Signal.of_int ~width:3 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] size is greater than what can be addressed by read port"
     (size          16)
     (address_width 3)) |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8 |]
      ~read_addresses:[| Signal.of_int ~width:5 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of read and write addresses differ"
     (write_address_width 4)
     (read_address_width  5)) |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:
        [| { (write_port 4 8) with write_clock = Signal.of_int ~width:2 0 } |]
      ~read_addresses:[| Signal.of_int ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of clock must be 1"
     (port               0)
     (write_enable_width 1)) |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:
        [| { (write_port 4 8) with write_enable = Signal.of_int ~width:2 0 } |]
      ~read_addresses:[| Signal.of_int ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of write enable must be 1"
     (port               0)
     (write_enable_width 2)) |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[||]
      ~read_addresses:[| Signal.of_int ~width:4 0 |]);
  [%expect {| "[Signal.multiport_memory] requires at least one write port" |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory 16 ~write_ports:[| write_port 4 8 |] ~read_addresses:[||]);
  [%expect {| "[Signal.multiport_memory] requires at least one read port" |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8 |]
      ~read_addresses:[| Signal.of_int ~width:4 0; Signal.of_int ~width:5 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of read address is inconsistent"
     (port               1)
     (read_address_width 5)
     (expected           4)) |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8; write_port 5 8 |]
      ~read_addresses:[| Signal.of_int ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of write address is inconsistent"
     (port                1)
     (write_address_width 5)
     (expected            4)) |}];
  require_does_raise [%here] (fun () ->
    Signal.multiport_memory
      16
      ~write_ports:[| write_port 4 8; write_port 4 16 |]
      ~read_addresses:[| Signal.of_int ~width:4 0 |]);
  [%expect
    {|
    ("[Signal.multiport_memory] width of write data is inconsistent"
     (port             1)
     (write_data_width 16)
     (expected         8)) |}]
;;

let%expect_test "sexp" =
  let sexp_of_signal = Signal.sexp_of_signal_recursive ~depth:2 in
  let memory =
    Signal.multiport_memory
      32
      ~write_ports:[| write_port 5 12; write_port 5 12 |]
      ~read_addresses:[| Signal.of_int ~width:5 0; Signal.of_int ~width:5 0 |]
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
           (value 0b00000))))))) |}]
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

        /* signal declarations */
        reg [14:0] _7[0:31];
        wire [14:0] _8;

        /* logic */
        always @(posedge clock) begin
            if (write_enable)
                _7[write_address] <= write_data;
        end
        assign _8 = _7[read_address];

        /* aliases */

        /* output assignments */
        assign q0 = _8;

    endmodule |}]
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
        write_enable2,
        write_data2,
        write_address2,
        clock2,
        write_enable,
        write_data,
        write_address,
        clock,
        read_address2,
        read_address,
        q0,
        q1
    );

        input write_enable2;
        input [14:0] write_data2;
        input [4:0] write_address2;
        input clock2;
        input write_enable;
        input [14:0] write_data;
        input [4:0] write_address;
        input clock;
        input [4:0] read_address2;
        input [4:0] read_address;
        output [14:0] q0;
        output [14:0] q1;

        /* signal declarations */
        wire [14:0] _14;
        reg [14:0] _13[0:31];
        wire [14:0] _15;

        /* logic */
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

        /* aliases */

        /* output assignments */
        assign q0 = _15;
        assign q1 = _14;

    endmodule |}]
;;

let dual_port ?(collision_mode = Ram.Collision_mode.Read_before_write) () =
  let read_data =
    Ram.create
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
        [| { Ram.Read_port.read_clock = Signal.input "read_clock1" 1
           ; read_address = Signal.input "read_address1" 5
           ; read_enable = Signal.input "read_enable1" 1
           }
         ; { read_clock = Signal.input "read_clock2" 1
           ; read_address = Signal.input "read_address2" 5
           ; read_enable = Signal.input "read_enable2" 1
           }
        |]
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
        read_address2,
        read_address1,
        q0,
        q1
    );

        input read_enable2;
        input read_clock2;
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
        input [4:0] read_address2;
        input [4:0] read_address1;
        output [14:0] q0;
        output [14:0] q1;

        /* signal declarations */
        wire [14:0] _20 = 15'b000000000000000;
        wire [14:0] _19 = 15'b000000000000000;
        wire [14:0] _18;
        reg [14:0] _21;
        wire [14:0] _24 = 15'b000000000000000;
        wire [14:0] _23 = 15'b000000000000000;
        reg [14:0] _17[0:31];
        wire [14:0] _22;
        reg [14:0] _25;

        /* logic */
        assign _18 = _17[read_address2];
        always @(posedge read_clock2) begin
            if (read_enable2)
                _21 <= _18;
        end
        always @(posedge write_clock1) begin
            if (write_enable1)
                _17[write_address1] <= write_data1;
        end
        always @(posedge write_clock2) begin
            if (write_enable2)
                _17[write_address2] <= write_data2;
        end
        assign _22 = _17[read_address1];
        always @(posedge read_clock1) begin
            if (read_enable1)
                _25 <= _22;
        end

        /* aliases */

        /* output assignments */
        assign q0 = _25;
        assign q1 = _21;

    endmodule |}]
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
            read_enable1 : in std_logic;
            read_clock1 : in std_logic;
            write_enable2 : in std_logic;
            write_data2 : in std_logic_vector (14 downto 0);
            write_address2 : in std_logic_vector (4 downto 0);
            write_clock2 : in std_logic;
            write_enable1 : in std_logic;
            write_data1 : in std_logic_vector (14 downto 0);
            write_address1 : in std_logic_vector (4 downto 0);
            write_clock1 : in std_logic;
            read_address2 : in std_logic_vector (4 downto 0);
            read_address1 : in std_logic_vector (4 downto 0);
            q0 : out std_logic_vector (14 downto 0);
            q1 : out std_logic_vector (14 downto 0)
        );
    end entity;

    architecture rtl of multi_port_memory is

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
        constant hc_20 : std_logic_vector (14 downto 0) := "000000000000000";
        constant hc_19 : std_logic_vector (14 downto 0) := "000000000000000";
        signal hc_18 : std_logic_vector (14 downto 0);
        signal hc_21 : std_logic_vector (14 downto 0);
        constant hc_24 : std_logic_vector (14 downto 0) := "000000000000000";
        constant hc_23 : std_logic_vector (14 downto 0) := "000000000000000";
        type hc_17_type is array (0 to 31) of std_logic_vector(14 downto 0);
        signal hc_17 : hc_17_type;
        signal hc_22 : std_logic_vector (14 downto 0);
        signal hc_25 : std_logic_vector (14 downto 0);

    begin

        -- logic
        hc_18 <= hc_17(to_integer(hc_uns(read_address2)));
        process (read_clock2) begin
            if rising_edge(read_clock2) then
                if read_enable2 = '1' then
                    hc_21 <= hc_18;
                end if;
            end if;
        end process;
        process (write_clock1) begin
            if rising_edge(write_clock1) then
                if write_enable1 = '1' then
                    hc_17(to_integer(hc_uns(write_address1))) <= write_data1;
                end if;
            end if;
        end process;
        process (write_clock2) begin
            if rising_edge(write_clock2) then
                if write_enable2 = '1' then
                    hc_17(to_integer(hc_uns(write_address2))) <= write_data2;
                end if;
            end if;
        end process;
        hc_22 <= hc_17(to_integer(hc_uns(read_address1)));
        process (read_clock1) begin
            if rising_edge(read_clock1) then
                if read_enable1 = '1' then
                    hc_25 <= hc_22;
                end if;
            end if;
        end process;

        -- aliases

        -- output assignments
        q0 <= hc_25;
        q1 <= hc_21;

    end architecture; |}]
;;

let%expect_test "simulation - write and read data on both ports" =
  let circuit = dual_port () in
  let simulator = Cyclesim.create circuit in
  let waves, simulator = Waves.Waveform.create simulator in
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
  write_address1 := Bits.of_int ~width:5 3;
  write_data1 := Bits.of_int ~width:15 100;
  Cyclesim.cycle simulator;
  write_address1 := Bits.of_int ~width:5 4;
  write_data1 := Bits.of_int ~width:15 640;
  Cyclesim.cycle simulator;
  write_enable1 := Bits.gnd;
  (* read on port 1 *)
  Cyclesim.cycle simulator;
  read_address1 := Bits.of_int ~width:5 3;
  read_enable1 := Bits.vdd;
  Cyclesim.cycle simulator;
  (* read on port 2 *)
  read_enable1 := Bits.gnd;
  read_address2 := Bits.of_int ~width:5 3;
  read_enable2 := Bits.vdd;
  Cyclesim.cycle simulator;
  read_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  (* read on ports 1 and 2 *)
  read_enable1 := Bits.vdd;
  read_enable2 := Bits.vdd;
  read_address1 := Bits.of_int ~width:5 4;
  read_address2 := Bits.of_int ~width:5 4;
  Cyclesim.cycle simulator;
  read_enable1 := Bits.gnd;
  read_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  Waves.Waveform.print ~display_height:42 ~display_width:86 ~wave_width:2 waves;
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
    └──────────────────┘└────────────────────────────────────────────────────────────────┘ |}]
;;

let%expect_test "simulation - write on both ports - highest indexed port wins" =
  let circuit = dual_port () in
  let simulator = Cyclesim.create circuit in
  let waves, simulator = Waves.Waveform.create simulator in
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
  write_address1 := Bits.of_int ~width:5 9;
  write_data1 := Bits.of_int ~width:15 100;
  write_enable2 := Bits.vdd;
  write_address2 := Bits.of_int ~width:5 9;
  write_data2 := Bits.of_int ~width:15 200;
  Cyclesim.cycle simulator;
  write_enable1 := Bits.gnd;
  write_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  read_enable1 := Bits.vdd;
  read_address1 := Bits.of_int ~width:5 9;
  read_enable2 := Bits.vdd;
  read_address2 := Bits.of_int ~width:5 9;
  Cyclesim.cycle simulator;
  read_enable1 := Bits.gnd;
  read_enable2 := Bits.gnd;
  Cyclesim.cycle simulator;
  Waves.Waveform.print ~display_height:42 ~display_width:60 ~wave_width:2 waves;
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
    └─────────────┘└───────────────────────────────────────────┘ |}]
;;

let%expect_test "simulation - demonstrate collision modes" =
  let test collision_mode =
    let circuit = dual_port ~collision_mode () in
    let simulator = Cyclesim.create circuit in
    let waves, simulator = Waves.Waveform.create simulator in
    let write_enable1 = Cyclesim.in_port simulator "write_enable1" in
    let write_address1 = Cyclesim.in_port simulator "write_address1" in
    let write_data1 = Cyclesim.in_port simulator "write_data1" in
    let read_address1 = Cyclesim.in_port simulator "read_address1" in
    let read_enable1 = Cyclesim.in_port simulator "read_enable1" in
    Cyclesim.reset simulator;
    write_enable1 := Bits.vdd;
    write_address1 := Bits.of_int ~width:5 13;
    write_data1 := Bits.of_int ~width:15 10;
    Cyclesim.cycle simulator;
    write_enable1 := Bits.vdd;
    write_address1 := Bits.of_int ~width:5 13;
    write_data1 := Bits.of_int ~width:15 20;
    read_enable1 := Bits.vdd;
    read_address1 := Bits.of_int ~width:5 13;
    Cyclesim.cycle simulator;
    write_enable1 := Bits.gnd;
    read_enable1 := Bits.gnd;
    Cyclesim.cycle simulator;
    Waves.Waveform.print ~display_height:42 ~display_width:60 ~wave_width:2 waves
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
    └─────────────┘└───────────────────────────────────────────┘ |}];
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
    └─────────────┘└───────────────────────────────────────────┘ |}]
;;
