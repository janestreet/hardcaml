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

let%expect_test "Scope.sub_scope applied correctly" =
  let circuit =
    let q =
      let scope = Scope.create () in
      let d = wireof (input "d" 1) in
      let scope = Scope.sub_scope scope "foo" in
      let bar = Scope.naming scope (d +:. 1) "bar" in
      let baz =
        let scope = Scope.sub_scope scope "deeper" in
        Scope.naming scope (bar +:. 1) "baz"
      in
      baz -:. 1 -:. 1
    in
    Circuit.create_exn ~name:"test" [ output "q" q ]
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

        wire signal_const;
        wire signal_wire;
        wire foo$bar;
        wire foo$deeper$baz;
        wire signal_sub;
        wire signal_sub_1;
        assign signal_const = 1'b1;
        assign signal_wire = d;
        assign foo$bar = signal_wire + signal_const;
        assign foo$deeper$baz = foo$bar + signal_const;
        assign signal_sub = foo$deeper$baz - signal_const;
        assign signal_sub_1 = signal_sub - signal_const;
        assign q = signal_sub_1;

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

        wire signal_const;
        wire signal_wire;
        wire foo$bar;
        wire foo$deeper$baz;
        wire signal_sub;
        wire signal_sub_1;
        assign signal_const = 1'b1;
        assign signal_wire = d;
        assign foo$bar = signal_wire + signal_const;
        assign foo$deeper$baz = foo$bar + signal_const;
        assign signal_sub = foo$deeper$baz - signal_const;
        assign signal_sub_1 = signal_sub - signal_const;
        assign q = signal_sub_1;

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

        signal signal_const : std_logic;
        signal signal_wire : std_logic;
        signal \foo$bar\ : std_logic;
        signal \foo$deeper$baz\ : std_logic;
        signal signal_sub : std_logic;
        signal signal_sub_1 : std_logic;

    begin

        signal_const <= '1';
        signal_wire <= d;
        \foo$bar\ <= (unsigned(std_logic_vector'("" & signal_wire)) + unsigned(std_logic_vector'("" & signal_const))) ?= "1";
        \foo$deeper$baz\ <= (unsigned(std_logic_vector'("" & \foo$bar\)) + unsigned(std_logic_vector'("" & signal_const))) ?= "1";
        signal_sub <= (unsigned(std_logic_vector'("" & \foo$deeper$baz\)) - unsigned(std_logic_vector'("" & signal_const))) ?= "1";
        signal_sub_1 <= (unsigned(std_logic_vector'("" & signal_sub)) - unsigned(std_logic_vector'("" & signal_const))) ?= "1";
        q <= signal_sub_1;

    end architecture;
    |}]
;;

let%expect_test "sva sequence formulas" =
  let module Formula = Property.Sva.Formula in
  let open Formula in
  let print_property (formula : (string, [> property ]) Formula.t) =
    formula
    |> Property.Sva.asserts
    |> Property.Sva.to_rope ~proposition:Rope.of_string
    |> Rope.to_string
    |> print_endline
  in
  show_raise (fun () ->
    ignore (Sequence_range.bounded_exn { from = -1; to_ = 1 } : Sequence_range.t));
  [%expect {| (raised ("SVA sequence range must be non-negative" (n -1))) |}];
  show_raise (fun () -> ignore (Sequence_range.at_least_exn (-1) : Sequence_range.t));
  [%expect {| (raised ("SVA sequence range must be non-negative" (n -1))) |}];
  show_raise (fun () -> ignore (Sequence_range.at_most_exn (-1) : Sequence_range.t));
  [%expect {| (raised ("SVA sequence range must be non-negative" (n -1))) |}];
  show_raise (fun () ->
    ignore (Sequence_range.bounded_exn { from = 3; to_ = 1 } : Sequence_range.t));
  [%expect
    {|
    (raised (
      "SVA sequence range must be increasing" (
        range (
          (from 3)
          (to_  1)))))
    |}];
  print_property
    (Delay (Some (Atom "a"), Sequence_range.bounded_exn { from = 0; to_ = 1 }, Atom "b"));
  [%expect {| assert property ((a) ##[0:1] (b)) |}];
  print_property
    (Delay (Some (Atom "a"), Sequence_range.bounded_exn { from = 1; to_ = 3 }, Atom "b"));
  [%expect {| assert property ((a) ##[1:3] (b)) |}];
  print_property (Delay (None, Sequence_range.at_least_exn 2, Atom "a"));
  [%expect {| assert property (##[2:$] (a)) |}];
  print_property (Delay (None, Sequence_range.at_most_exn 4, Atom "a"));
  [%expect {| assert property (##[0:4] (a)) |}];
  print_property (Repeat (Atom "a", Sequence_range.bounded_exn { from = 2; to_ = 5 }));
  [%expect {| assert property ((a) [*2:5]) |}];
  print_property (Repeat (Atom "a", Sequence_range.at_least_exn 0));
  [%expect {| assert property ((a) [*]) |}];
  print_property (Repeat (Atom "a", Sequence_range.at_least_exn 1));
  [%expect {| assert property ((a) [+]) |}];
  print_property (Repeat (Atom "a", Sequence_range.at_least_exn 2));
  [%expect {| assert property ((a) [*2:$]) |}];
  print_property (Repeat (Atom "a", Sequence_range.at_most_exn 4));
  [%expect {| assert property ((a) [*0:4]) |}];
  print_property
    (Throughout
       ( "a"
       , Delay
           (Some (Atom "b"), Sequence_range.bounded_exn { from = 1; to_ = 3 }, Atom "c")
       ));
  [%expect {| assert property ((a) throughout ((b) ##[1:3] (c))) |}];
  print_property
    (Overlapping_implication
       ( Delay
           (Some (Atom "a"), Sequence_range.bounded_exn { from = 1; to_ = 3 }, Atom "b")
       , Always (Atom "c", None) ));
  [%expect {| assert property (((a) ##[1:3] (b)) |-> (always (c))) |}]
;;

let%expect_test "sva sequence properties are emitted as system verilog assertions" =
  let module Formula = Property.Sva.Formula in
  let scope =
    Scope.create ~trace_properties:(Scope.Trace_properties.create ~enable_sva:true ()) ()
  in
  let clk = input "clk" 1 in
  let a = input "a" 1 in
  let b = input "b" 1 in
  let c = input "c" 1 in
  let add_property name (formula : (Signal.t, [> Formula.property ]) Formula.t) =
    Scope.add_sva_property scope name (Property.Sva.asserts formula)
  in
  add_property
    "delayed"
    (Delay
       ( Some (Atom a)
       , Formula.Sequence_range.bounded_exn { from = 1; to_ = 3 }
       , Formula.Atom b ));
  add_property
    "repeat_property"
    (Formula.Repeat
       (Formula.Atom a, Formula.Sequence_range.bounded_exn { from = 2; to_ = 5 }));
  add_property
    "zero_or_more"
    (Formula.Repeat (Formula.Atom a, Formula.Sequence_range.at_least_exn 0));
  add_property
    "one_or_more"
    (Formula.Repeat (Formula.Atom a, Formula.Sequence_range.at_least_exn 1));
  add_property
    "throughout_property"
    (Formula.Throughout
       ( a
       , Formula.Delay
           ( Some (Formula.Atom b)
           , Formula.Sequence_range.bounded_exn { from = 1; to_ = 3 }
           , Formula.Atom c ) ));
  add_property
    "overlapping_implication"
    (Formula.Overlapping_implication
       ( Formula.Delay
           ( Some (Formula.Atom a)
           , Formula.Sequence_range.bounded_exn { from = 1; to_ = 3 }
           , Formula.Atom b )
       , Formula.Always (Formula.Atom c, None) ));
  Scope.add_sva_property
    scope
    "clocked_leading_delay"
    (Property.Sva.asserts
       ~clocked:(Property.Clocked.create Rising clk)
       (Formula.Delay (None, Formula.Sequence_range.at_least_exn 2, Formula.Atom a)));
  let circuit =
    Circuit.create_exn
      ~config:(Scope.circuit_config_with_properties scope)
      ~name:"sequence_properties"
      [ output "q" a ]
  in
  Rtl.print Systemverilog circuit;
  [%expect
    {|
    module sequence_properties (
        c,
        b,
        clk,
        a,
        q
    );

        input c;
        input b;
        input clk;
        input a;
        output q;

        zero_or_more: assert property ((a) [*]);
        repeat_property: assert property ((a) [*2:5]);
        throughout_property: assert property ((a) throughout ((b) ##[1:3] (c)));
        overlapping_implication: assert property (((a) ##[1:3] (b)) |-> (always (c)));
        delayed: assert property ((a) ##[1:3] (b));
        one_or_more: assert property ((a) [+]);
        clocked_leading_delay: assert property (@(posedge clk) ##[2:$] (a));
        assign q = a;

    endmodule
    |}]
;;

let%expect_test "ltl properties are emitted as system verilog assertions" =
  let scope =
    Scope.create ~trace_properties:(Scope.Trace_properties.create ~enable_sva:true ()) ()
  in
  let clk = input "clk" 1 in
  let clear = input "clear" 1 in
  let d = input "d" 1 in
  let d_is_high = Scope.make_ltl_ap scope "d_is_high" d in
  (* Property converted from LTL *)
  Scope.add_sva_property
    scope
    "d_eventually_high"
    (Property.LTL.(asserts (Formula.f d_is_high)) |> Property.Sva.of_ltl);
  (* Property written in Sva *)
  Scope.add_sva_property scope "assert_d_never_high" Property.Sva.(asserts (Atom ~:d));
  Scope.add_sva_property scope "assume_d_never_high" Property.Sva.(assume (Atom ~:d));
  Scope.add_sva_property scope "cover_d_high" Property.Sva.(cover (Atom d));
  Scope.add_sva_property
    scope
    "some_clocked_thing"
    Property.Sva.(
      asserts
        ~clocked:(Property.Clocked.create Rising clk)
        (Or (S_eventually (Atom d, None), Nexttime (Atom d, None))));
  Scope.add_sva_property
    scope
    "implies_something"
    Property.Sva.(
      asserts
        ~clocked:(Property.Clocked.create Rising clk ~disable_iff:clear)
        (Property_implication
           (Nexttime (Atom d, Some 2), Always (Atom d, Some { from = 1; to_ = 3 }))));
  let circuit =
    Circuit.create_exn
      ~config:(Scope.circuit_config_with_properties scope)
      ~name:"properties"
      [ output "q" d ]
  in
  Rtl.print Systemverilog circuit;
  [%expect
    {|
    module properties (
        clear,
        clk,
        d,
        q
    );

        input clear;
        input clk;
        input d;
        output q;

        wire signal_not;
        wire signal_not_1;
        wire ap$d_is_high;
        assign signal_not = ~ d;
        assign signal_not_1 = ~ d;
        assign ap$d_is_high = d;
        assume_d_never_high: assume property (signal_not);
        implies_something: assert property (@(posedge clk) disable iff (clear) (nexttime [2] (d)) implies (always [1:3] (d)));
        some_clocked_thing: assert property (@(posedge clk) (s_eventually (d)) or (nexttime (d)));
        cover_d_high: cover property (d);
        d_eventually_high: assert property (s_eventually (ap$d_is_high));
        assert_d_never_high: assert property (signal_not_1);
        assign q = d;

    endmodule
    |}]
;;

(* SVA properties within hierarchies *)

module I = (val Types.value ~name:"i" 1)
module O = (val Types.value ~name:"o" 1)

module Inner = struct
  let create scope (i : _ I.t) : _ O.t =
    Scope.add_sva_property scope "inner" Property.Sva.(asserts Formula.(Atom ~:i));
    i
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical create
  ;;
end

let create scope (i : _ I.t) : _ O.t =
  Scope.add_sva_property scope "outer" Property.Sva.(asserts Formula.(Atom (i +:. 1)));
  Inner.hierarchical ~scope i
;;

let%expect_test "sva in hierarchy" =
  let module Circ = Circuit.With_interface (I) (O) in
  let scope =
    Scope.create ~trace_properties:(Scope.Trace_properties.create ~enable_sva:true ()) ()
  in
  let circ =
    Circ.create_exn
      ~name:"outer"
      ~config:(Scope.circuit_config_with_properties scope)
      (create scope)
  in
  Rtl.print ~database:(Scope.circuit_database scope) Verilog circ;
  [%expect
    {|
    module test_rtl_gen (
        i,
        o
    );

        input i;
        output o;

        wire signal_not;
        wire signal_wire;
        assign signal_not = ~ signal_wire;
        assign signal_wire = i;
        inner: assert property (signal_not);
        assign o = signal_wire;

    endmodule
    module outer (
        i,
        o
    );

        input i;
        output o;

        wire signal_const;
        wire signal_add;
        wire signal_wire;
        wire signal_inst;
        wire signal_wire_1;
        assign signal_const = 1'b1;
        assign signal_add = signal_wire + signal_const;
        assign signal_wire = i;
        test_rtl_gen
            test_rtl_gen
            ( .i(signal_wire),
              .o(signal_inst) );
        assign signal_wire_1 = signal_inst;
        outer: assert property (signal_add);
        assign o = signal_wire_1;

    endmodule
    |}]
;;

module Inlined_scope_with_sva = struct
  let create scope (i : _ I.t) : _ O.t =
    Scope.add_sva_property scope "inline_child" Property.Sva.(asserts Formula.(Atom i));
    i
  ;;

  let hierarchical =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ~how_to_instantiate:Hierarchy.How_to_instantiate.Inlined_in_scope
      ~name:"inline_child"
      create
  ;;
end

let create_with_inlined_scope_sva scope (i : _ I.t) : _ O.t =
  Scope.add_sva_property scope "parent" Property.Sva.(cover Formula.(Atom i));
  Inlined_scope_with_sva.hierarchical ~scope i
;;

let%expect_test "sva in inlined sub-scope" =
  let module Circ = Circuit.With_interface (I) (O) in
  let scope =
    Scope.create ~trace_properties:(Scope.Trace_properties.create ~enable_sva:true ()) ()
  in
  let circ =
    Circ.create_exn
      ~name:"top"
      ~config:(Scope.circuit_config_with_properties scope)
      (create_with_inlined_scope_sva scope)
  in
  let sva_property_names =
    Circuit.sva_properties circ
    |> Option.value_exn
    |> Hashtbl.keys
    |> List.sort ~compare:String.compare
  in
  Stdio.print_s [%message (sva_property_names : string list)];
  [%expect {| (sva_property_names (inline_child$inline_child parent)) |}]
;;

(* Properties can be tagged with a group, and only properties in the enabled groups (and
   of an enabled type) are traced. *)

let group_a = Property_manager.Group.create "group_a"
let group_b = Property_manager.Group.create "group_b"

let create_with_groups scope =
  let d = input "d" 1 in
  Scope.add_sva_property ~group:group_a scope "in_group_a" Property.Sva.(asserts (Atom d));
  Scope.add_sva_property
    ~group:group_b
    scope
    "in_group_b"
    Property.Sva.(asserts (Atom ~:d));
  Scope.add_sva_property scope "in_default_group" Property.Sva.(cover (Atom d));
  Circuit.create_exn
    ~config:(Scope.circuit_config_with_properties scope)
    ~name:"groups"
    [ output "q" d ]
;;

let print_with_enabled_groups groups =
  let scope =
    Scope.create
      ~trace_properties:(Scope.Trace_properties.create ~enable_sva:true ~groups ())
      ()
  in
  Rtl.print Systemverilog (create_with_groups scope)
;;

let%expect_test "only properties in enabled groups are traced" =
  (* Just one group. *)
  print_with_enabled_groups [ group_a ];
  [%expect
    {|
    module groups (
        d,
        q
    );

        input d;
        output q;

        in_group_a: assert property (d);
        assign q = d;

    endmodule
    |}];
  (* A custom group and the default group. *)
  print_with_enabled_groups [ group_b; Property_manager.Group.default ];
  [%expect
    {|
    module groups (
        d,
        q
    );

        input d;
        output q;

        wire signal_not;
        assign signal_not = ~ d;
        in_group_b: assert property (signal_not);
        in_default_group: cover property (d);
        assign q = d;

    endmodule
    |}];
  (* No groups enabled - no properties are emitted. *)
  print_with_enabled_groups [];
  [%expect
    {|
    module groups (
        d,
        q
    );

        input d;
        output q;

        assign q = d;

    endmodule
    |}];
  (* The special [all] group traces every property, regardless of its own group. *)
  print_with_enabled_groups [ Property_manager.Group.all ];
  [%expect
    {|
    module groups (
        d,
        q
    );

        input d;
        output q;

        wire signal_not;
        assign signal_not = ~ d;
        in_group_a: assert property (d);
        in_group_b: assert property (signal_not);
        in_default_group: cover property (d);
        assign q = d;

    endmodule
    |}]
;;

let%expect_test "[enable_sva] gates whether sva properties are emitted" =
  let print ~enable_sva =
    let scope =
      Scope.create ~trace_properties:(Scope.Trace_properties.create ~enable_sva ()) ()
    in
    Rtl.print Systemverilog (create_with_groups scope)
  in
  print ~enable_sva:false;
  [%expect
    {|
    module groups (
        d,
        q
    );

        input d;
        output q;

        assign q = d;

    endmodule
    |}];
  print ~enable_sva:true;
  [%expect
    {|
    module groups (
        d,
        q
    );

        input d;
        output q;

        in_default_group: cover property (d);
        assign q = d;

    endmodule
    |}]
;;
