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
        wire _4;
        assign _4 = i | j;
        assign o = _4;

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

        signal \_4\ : std_logic;
        attribute boolattr of \_4\ : signal is true;
        attribute intattr of \_4\ : signal is 123;

    begin

        \_4\ <= i or j;
        o <= \_4\;

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

        wire _2;
        assign _2 = a;
        assign b = _2;

    endmodule
    module attributes (
        i,
        o
    );

        input i;
        (* nameattr *)
        output o;

        wire _4;
        wire _2;
        (* stringattr="foo" *)
        inner
            inner
            ( .a(i),
              .b(_4) );
        assign _2 = _4;
        assign o = _2;

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

        signal \_2\ : std_logic;

    begin

        \_2\ <= a;
        b <= \_2\;

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

        signal \_4\ : std_logic;
        attribute stringattr of inner : label is "foo";
        signal \_2\ : std_logic;

    begin

        inner: entity work.inner (rtl)
            port map ( a => i,
                       b => \_4\ );
        \_2\ <= \_4\;
        o <= \_2\;

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
        reg [7:0] _7[0:3];
        wire [7:0] _8;
        always @(posedge clk) begin
            if (write_enable)
                _7[write_address] <= write_data;
        end
        assign _8 = _7[read_address];
        assign q = _8;

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

        type \_7_type\ is array (0 to 3) of std_logic_vector(7 downto 0);
        signal \_7\ : \_7_type\;
        attribute on_mem of \_7\ : signal is 123;
        signal \_8\ : std_logic_vector(7 downto 0);

    begin

        process (clk) begin
            if rising_edge(clk) then
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

        wire _4/* I am a comment */;
        assign _4 = i | j;
        assign o = _4;

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

        signal \_4\ : std_logic;

    begin

        \_4\ <= i or j;
        o <= \_4\;

    end architecture;
    |}]
;;
