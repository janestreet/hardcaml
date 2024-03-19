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
      [ name_attr
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
        (* nameattr *)
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
    end entity;

    architecture rtl of attributes is

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
        signal hc_4 : std_logic;

    begin

        hc_4 <= hc_sl(hc_uns(i) or hc_uns(j));
        o <= hc_4;

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
        signal hc_4 : std_logic;

    begin

        hc_4 <= hc_sl(hc_uns(i) or hc_uns(j));
        o <= hc_4;

    end architecture;
    |}]
;;
