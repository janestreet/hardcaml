open Core
open Hardcaml
open Signal

let name_attr t = add_attribute t (Rtl_attribute.create "nameattr")

let string_attr t =
  add_attribute t (Rtl_attribute.create "stringattr" ~value:(String "foo"))
;;

let int_attr t = add_attribute t (Rtl_attribute.create "intattr" ~value:(Int 123))
let false_attr t = add_attribute t (Rtl_attribute.create "boolattr" ~value:(Bool false))
let true_attr t = add_attribute t (Rtl_attribute.create "boolattr" ~value:(Bool true))

let%expect_test "attributes on signals" =
  let circuit =
    Circuit.create_exn
      ~name:"attributes"
      [ true_attr
          (output
             "o"
             (true_attr
                (int_attr (string_attr (input "i" 1) |: false_attr (input "j" 1)))))
      ]
  in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module attributes (
        j,
        i,
        o
    );

        (* boolattr=0 *)
        input j;
        (* stringattr="foo" *)
        input i;
        (* boolattr=1 *)
        output o;

        (* boolattr=1,intattr=123 *)
        wire signal_or;
        assign signal_or = i | j;
        assign o = signal_or;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity attributes is
        port (
            j : in std_logic;
            i : in std_logic;
            o : out std_logic
        );
        attribute boolattr : boolean;
        attribute intattr : integer;
        attribute stringattr : string;
        attribute boolattr of j : signal is false;
        attribute stringattr of i : signal is "foo";
        attribute boolattr of o : signal is true;
    end entity;

    architecture rtl of attributes is

        signal signal_or : std_logic;
        attribute boolattr of signal_or : signal is true;
        attribute intattr of signal_or : signal is 123;

    begin

        signal_or <= i or j;
        o <= signal_or;

    end architecture;
    |}]
;;

module C = struct
  module I = struct
    type 'a t = { a : 'a } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { b : 'a } [@@deriving hardcaml]
  end

  let create _ (i : _ I.t) = { O.b = i.a }

  let hier scope =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical
      ~scope
      ~attributes:[ Rtl_attribute.create "stringattr" ~value:(String "foo") ]
      ~name:"inner"
      create
  ;;
end

let%expect_test "attributes on instantiations" =
  let scope = Scope.create () in
  let circuit =
    let i = input "i" 1 in
    let inner = C.hier scope { C.I.a = i } in
    Circuit.create_exn ~name:"attributes" [ name_attr (output "o" inner.b) ]
  in
  Testing.analyse_vhdl_and_verilog
    ~show:true
    ~database:(Scope.circuit_database scope)
    circuit;
  [%expect
    {|
    module inner (
        a,
        b
    );

        input a;
        output b;

        wire signal_wire;
        assign signal_wire = a;
        assign b = signal_wire;

    endmodule
    module attributes (
        i,
        o
    );

        input i;
        (* nameattr *)
        output o;

        wire signal_inst;
        wire signal_wire;
        (* stringattr="foo" *)
        inner
            inner
            ( .a(i),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        assign o = signal_wire;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity inner is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of inner is

        signal signal_wire : std_logic;

    begin

        signal_wire <= a;
        b <= signal_wire;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity attributes is
        port (
            i : in std_logic;
            o : out std_logic
        );
        attribute stringattr : string;
    end entity;

    architecture rtl of attributes is

        signal signal_inst : std_logic;
        attribute stringattr of inner : label is "foo";
        signal signal_wire : std_logic;

    begin

        inner: entity work.inner (rtl)
            port map ( a => i,
                       b => signal_inst );
        signal_wire <= signal_inst;
        o <= signal_wire;

    end architecture;
    |}]
;;

let%expect_test "attributes on memories" =
  let circuit =
    Circuit.create_exn
      ~name:"attributes"
      [ output
          "q"
          (multiport_memory
             4
             ~attributes:[ Rtl_attribute.create "on_mem" ~value:(Int 123) ]
             ~write_ports:
               [| { write_clock = input "clk" 1
                  ; write_address = input "write_address" 2
                  ; write_enable = input "write_enable" 1
                  ; write_data = input "write_data" 8
                  }
               |]
             ~read_addresses:[| input "read_address" 2 |]).(0)
      ]
  in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module attributes (
        write_enable,
        write_data,
        write_address,
        clk,
        read_address,
        q
    );

        input write_enable;
        input [7:0] write_data;
        input [1:0] write_address;
        input clk;
        input [1:0] read_address;
        output [7:0] q;

        (* on_mem=123 *)
        reg [7:0] signal_multiport_mem[0:3];
        wire [7:0] signal_mem_read_port;
        always @(posedge clk) begin
            if (write_enable)
                signal_multiport_mem[write_address] <= write_data;
        end
        assign signal_mem_read_port = signal_multiport_mem[read_address];
        assign q = signal_mem_read_port;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity attributes is
        port (
            write_enable : in std_logic;
            write_data : in std_logic_vector(7 downto 0);
            write_address : in std_logic_vector(1 downto 0);
            clk : in std_logic;
            read_address : in std_logic_vector(1 downto 0);
            q : out std_logic_vector(7 downto 0)
        );
        attribute on_mem : integer;
    end entity;

    architecture rtl of attributes is

        type signal_multiport_mem_type is array (0 to 3) of std_logic_vector(7 downto 0);
        signal signal_multiport_mem : signal_multiport_mem_type;
        attribute on_mem of signal_multiport_mem : signal is 123;
        signal signal_mem_read_port : std_logic_vector(7 downto 0);

    begin

        process (clk) begin
            if rising_edge(clk) then
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

let%expect_test "comments on signals" =
  let circuit =
    Circuit.create_exn
      ~name:"comments"
      [ output "o" (set_comment (input "i" 1 |: input "j" 1) "I am a comment") ]
  in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module comments (
        j,
        i,
        o
    );

        input j;
        input i;
        output o;

        wire signal_or/* I am a comment */;
        assign signal_or = i | j;
        assign o = signal_or;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity comments is
        port (
            j : in std_logic;
            i : in std_logic;
            o : out std_logic
        );
    end entity;

    architecture rtl of comments is

        signal signal_or : std_logic;

    begin

        signal_or <= i or j;
        o <= signal_or;

    end architecture;
    |}]
;;
