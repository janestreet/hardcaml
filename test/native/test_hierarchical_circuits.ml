open Async
open Hardcaml
open Expect_test_helpers_async

let outer = Hardcaml_test.Test_hierarchical_circuits.outer

let%expect_test "To_file" =
  let%bind () =
    within_temp_dir (fun () ->
      let database = Circuit_database.create () in
      let circuit = outer ~db:database ~cause_exn:false ~share:true in
      Rtl.output Verilog circuit ~output_mode:(To_file "single_file.v") ~database;
      system "ls *.v; cat *.v")
  in
  [%expect
    {|
    single_file.v
    module inner (
        a,
        b
    );

        input a;
        output b;

        assign b = a;

    endmodule
    module middle (
        a,
        b
    );

        input a;
        output b;

        wire _5;
        wire _1;
        wire _6;
        wire _3;
        wire _7;
        inner
            the_inner
            ( .a(a),
              .b(_5) );
        assign _1 = _5;
        inner
            the_inner_1
            ( .a(a),
              .b(_6) );
        assign _3 = _6;
        assign _7 = _3 | _1;
        assign b = _7;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        wire _4;
        wire _2;
        middle
            the_middle
            ( .a(a),
              .b(_4) );
        assign _2 = _4;
        assign b = _2;

    endmodule
    |}];
  return ()
;;

let%expect_test "In_directory" =
  let%bind () =
    within_temp_dir (fun () ->
      let database = Circuit_database.create () in
      let circuit = outer ~db:database ~cause_exn:false ~share:true in
      Rtl.output Vhdl circuit ~output_mode:(In_directory ".") ~database;
      system "ls *.vhd; cat *.vhd")
  in
  [%expect
    {|
    inner.vhd
    middle.vhd
    outer.vhd
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

    begin

        b <= a;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity middle is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of middle is

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
        signal hc_5 : std_logic;
        signal hc_1 : std_logic;
        signal hc_6 : std_logic;
        signal hc_3 : std_logic;
        signal hc_7 : std_logic;

    begin

        the_inner: entity work.inner (rtl)
            port map ( a => a,
                       b => hc_5 );
        hc_1 <= hc_5;
        the_inner_1: entity work.inner (rtl)
            port map ( a => a,
                       b => hc_6 );
        hc_3 <= hc_6;
        hc_7 <= hc_sl(hc_uns(hc_3) or hc_uns(hc_1));
        b <= hc_7;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity outer is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of outer is

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
        signal hc_2 : std_logic;

    begin

        the_middle: entity work.middle (rtl)
            port map ( a => a,
                       b => hc_4 );
        hc_2 <= hc_4;
        b <= hc_2;

    end architecture;
    |}];
  return ()
;;
