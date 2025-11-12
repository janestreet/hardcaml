open Core
open Hardcaml

let circuit =
  let module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; reset : 'a
      ; foo : 'a
      ; bar : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module O = struct
    type 'a t =
      { zoo : 'a
      ; moo : 'a [@bits 2]
      ; sum : 'a [@bits 2]
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"temp" (fun (i : _ I.t) ->
      let open Signal.Unoptimized in
      let spec = Signal.Reg_spec.create ~clock:i.clock ~clear:i.clear ~reset:i.reset () in
      let zoo = i.foo +: i.bar in
      let sum = Unsigned.(i.foo +: i.bar) in
      let coo = i.foo -: ~:(i.bar) -- "coo" in
      let doo = zoo |: ~:(coo -- "noo") -- "zoo" in
      let moo = mux2 zoo (doo @: doo) (zoo @: coo) in
      let moo =
        Signal.wireof
          ((moo |: ones 2 -- "aaa" -- "bbb" |: ones 2) -- "ccc" -- "ddd" -- "eee" -- "fff")
      in
      let zoo = Signal.reg spec ~enable:coo zoo in
      { zoo; moo; sum })
  in
  lazy circuit
;;

let%expect_test "simple example" =
  Testing.analyse_vhdl_and_verilog ~show:true (force circuit);
  [%expect
    {|
    module temp (
        clear,
        reset,
        clock,
        bar,
        foo,
        zoo,
        moo,
        sum
    );

        input clear;
        input reset;
        input clock;
        input bar;
        input foo;
        output zoo;
        output [1:0] moo;
        output [1:0] sum;

        wire [1:0] _17;
        wire gnd;
        wire [1:0] _16;
        wire [1:0] _18;
        wire [1:0] _29;
        wire [1:0] bbb;
        wire [1:0] aaa;
        wire zoo_0;
        wire _24;
        wire [1:0] _25;
        wire [1:0] _22;
        wire [1:0] _26;
        wire [1:0] _28;
        wire [1:0] fff;
        wire [1:0] eee;
        wire [1:0] ddd;
        wire [1:0] ccc;
        wire [1:0] _2;
        wire _20;
        wire noo;
        wire coo;
        wire _32;
        wire _5;
        wire _7;
        wire _9;
        wire _11;
        wire _13;
        wire _19;
        reg _33;
        assign _17 = { gnd,
                       _11 };
        assign gnd = 1'b0;
        assign _16 = { gnd,
                       _13 };
        assign _18 = _16 + _17;
        assign _29 = 2'b11;
        assign bbb = 2'b11;
        assign zoo_0 = ~ noo;
        assign _24 = _19 | zoo_0;
        assign _25 = { _24,
                       _24 };
        assign _22 = { _19,
                       noo };
        assign _26 = _19 ? _25 : _22;
        assign _28 = _26 | bbb;
        assign fff = _28 | _29;
        assign _2 = fff;
        assign _20 = ~ _11;
        assign noo = _13 - _20;
        assign _32 = 1'b0;
        assign _5 = clear;
        assign _7 = reset;
        assign _9 = clock;
        assign _11 = bar;
        assign _13 = foo;
        assign _19 = _13 + _11;
        always @(posedge _9 or posedge _7) begin
            if (_7)
                _33 <= _32;
            else
                if (_5)
                    _33 <= _32;
                else
                    if (noo)
                        _33 <= _19;
        end
        assign aaa = bbb;
        assign eee = fff;
        assign ddd = fff;
        assign ccc = fff;
        assign coo = noo;
        assign zoo = _33;
        assign moo = _2;
        assign sum = _18;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity temp is
        port (
            clear : in std_logic;
            reset : in std_logic;
            clock : in std_logic;
            bar : in std_logic;
            foo : in std_logic;
            zoo : out std_logic;
            moo : out std_logic_vector(1 downto 0);
            sum : out std_logic_vector(1 downto 0)
        );
    end entity;

    architecture rtl of temp is

        signal \_17\ : std_logic_vector(1 downto 0);
        signal gnd : std_logic;
        signal \_16\ : std_logic_vector(1 downto 0);
        signal \_18\ : std_logic_vector(1 downto 0);
        signal \_29\ : std_logic_vector(1 downto 0);
        signal bbb : std_logic_vector(1 downto 0);
        signal aaa : std_logic_vector(1 downto 0);
        signal zoo_0 : std_logic;
        signal \_24\ : std_logic;
        signal \_25\ : std_logic_vector(1 downto 0);
        signal \_22\ : std_logic_vector(1 downto 0);
        signal \_26\ : std_logic_vector(1 downto 0);
        signal \_28\ : std_logic_vector(1 downto 0);
        signal fff : std_logic_vector(1 downto 0);
        signal eee : std_logic_vector(1 downto 0);
        signal ddd : std_logic_vector(1 downto 0);
        signal ccc : std_logic_vector(1 downto 0);
        signal \_2\ : std_logic_vector(1 downto 0);
        signal \_20\ : std_logic;
        signal noo : std_logic;
        signal coo : std_logic;
        signal \_32\ : std_logic;
        signal \_5\ : std_logic;
        signal \_7\ : std_logic;
        signal \_9\ : std_logic;
        signal \_11\ : std_logic;
        signal \_13\ : std_logic;
        signal \_19\ : std_logic;
        signal \_33\ : std_logic;

    begin

        \_17\ <= gnd & \_11\;
        gnd <= '0';
        \_16\ <= gnd & \_13\;
        \_18\ <= std_logic_vector(unsigned(\_16\) + unsigned(\_17\));
        \_29\ <= "11";
        bbb <= "11";
        zoo_0 <= not noo;
        \_24\ <= \_19\ or zoo_0;
        \_25\ <= \_24\ & \_24\;
        \_22\ <= \_19\ & noo;
        with to_integer(unsigned(std_logic_vector'("" & \_19\))) select \_26\ <=
            \_22\ when 0,
            \_25\ when others;
        \_28\ <= \_26\ or bbb;
        fff <= \_28\ or \_29\;
        \_2\ <= fff;
        \_20\ <= not \_11\;
        noo <= (unsigned(std_logic_vector'("" & \_13\)) - unsigned(std_logic_vector'("" & \_20\))) ?= "1";
        \_32\ <= '0';
        \_5\ <= clear;
        \_7\ <= reset;
        \_9\ <= clock;
        \_11\ <= bar;
        \_13\ <= foo;
        \_19\ <= (unsigned(std_logic_vector'("" & \_13\)) + unsigned(std_logic_vector'("" & \_11\))) ?= "1";
        process (\_9\, \_7\) begin
            if rising_edge(\_7\) then
                \_33\ <= \_32\;
            else
                if rising_edge(\_9\) then
                    if \_5\ = '1' then
                        \_33\ <= \_32\;
                    else
                        if noo = '1' then
                            \_33\ <= \_19\;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        aaa <= bbb;
        eee <= fff;
        ddd <= fff;
        ccc <= fff;
        coo <= noo;
        zoo <= \_33\;
        moo <= \_2\;
        sum <= \_18\;

    end architecture;
    |}]
;;

let%expect_test "apply port mangling to " =
  Testing.analyse_vhdl_and_verilog
    ~show:true
    ~config:{ Rtl.Config.default with two_state = true }
    (force circuit);
  [%expect
    {|
    module temp (
        clear,
        reset,
        clock,
        bar,
        foo,
        zoo,
        moo,
        sum
    );

        input clear;
        input reset;
        input clock;
        input bar;
        input foo;
        output zoo;
        output [1:0] moo;
        output [1:0] sum;

        bit [1:0] _17;
        bit gnd;
        bit [1:0] _16;
        bit [1:0] _18;
        bit [1:0] _29;
        bit [1:0] bbb;
        bit [1:0] aaa;
        bit zoo_0;
        bit _24;
        bit [1:0] _25;
        bit [1:0] _22;
        bit [1:0] _26;
        bit [1:0] _28;
        bit [1:0] fff;
        bit [1:0] eee;
        bit [1:0] ddd;
        bit [1:0] ccc;
        bit [1:0] _2;
        bit _20;
        bit noo;
        bit coo;
        bit _32;
        bit _5;
        bit _7;
        bit _9;
        bit _11;
        bit _13;
        bit _19;
        bit _33;
        assign _17 = { gnd,
                       _11 };
        assign gnd = 1'b0;
        assign _16 = { gnd,
                       _13 };
        assign _18 = _16 + _17;
        assign _29 = 2'b11;
        assign bbb = 2'b11;
        assign zoo_0 = ~ noo;
        assign _24 = _19 | zoo_0;
        assign _25 = { _24,
                       _24 };
        assign _22 = { _19,
                       noo };
        assign _26 = _19 ? _25 : _22;
        assign _28 = _26 | bbb;
        assign fff = _28 | _29;
        assign _2 = fff;
        assign _20 = ~ _11;
        assign noo = _13 - _20;
        assign _32 = 1'b0;
        assign _5 = clear;
        assign _7 = reset;
        assign _9 = clock;
        assign _11 = bar;
        assign _13 = foo;
        assign _19 = _13 + _11;
        always @(posedge _9 or posedge _7) begin
            if (_7)
                _33 <= _32;
            else
                if (_5)
                    _33 <= _32;
                else
                    if (noo)
                        _33 <= _19;
        end
        assign aaa = bbb;
        assign eee = fff;
        assign ddd = fff;
        assign ccc = fff;
        assign coo = noo;
        assign zoo = _33;
        assign moo = _2;
        assign sum = _18;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_bit.all;

    entity temp is
        port (
            clear : in bit;
            reset : in bit;
            clock : in bit;
            bar : in bit;
            foo : in bit;
            zoo : out bit;
            moo : out bit_vector(1 downto 0);
            sum : out bit_vector(1 downto 0)
        );
    end entity;

    architecture rtl of temp is
        -- Conversions
        function to_stdlogic(i : in bit) return std_logic is
        begin
            if i = '0' then
                return '0';
            else
                return '1';
            end if;
        end function;

        signal \_17\ : bit_vector(1 downto 0);
        signal gnd : bit;
        signal \_16\ : bit_vector(1 downto 0);
        signal \_18\ : bit_vector(1 downto 0);
        signal \_29\ : bit_vector(1 downto 0);
        signal bbb : bit_vector(1 downto 0);
        signal aaa : bit_vector(1 downto 0);
        signal zoo_0 : bit;
        signal \_24\ : bit;
        signal \_25\ : bit_vector(1 downto 0);
        signal \_22\ : bit_vector(1 downto 0);
        signal \_26\ : bit_vector(1 downto 0);
        signal \_28\ : bit_vector(1 downto 0);
        signal fff : bit_vector(1 downto 0);
        signal eee : bit_vector(1 downto 0);
        signal ddd : bit_vector(1 downto 0);
        signal ccc : bit_vector(1 downto 0);
        signal \_2\ : bit_vector(1 downto 0);
        signal \_20\ : bit;
        signal noo : bit;
        signal coo : bit;
        signal \_32\ : bit;
        signal \_5\ : bit;
        signal \_7\ : bit;
        signal \_9\ : bit;
        signal \_11\ : bit;
        signal \_13\ : bit;
        signal \_19\ : bit;
        signal \_33\ : bit;

    begin

        \_17\ <= gnd & \_11\;
        gnd <= '0';
        \_16\ <= gnd & \_13\;
        \_18\ <= bit_vector(unsigned(\_16\) + unsigned(\_17\));
        \_29\ <= "11";
        bbb <= "11";
        zoo_0 <= not noo;
        \_24\ <= \_19\ or zoo_0;
        \_25\ <= \_24\ & \_24\;
        \_22\ <= \_19\ & noo;
        with to_integer(unsigned'("" & \_19\)) select \_26\ <=
            \_22\ when 0,
            \_25\ when others;
        \_28\ <= \_26\ or bbb;
        fff <= \_28\ or \_29\;
        \_2\ <= fff;
        \_20\ <= not \_11\;
        noo <= (unsigned'("" & \_13\) - unsigned'("" & \_20\)) ?= "1";
        \_32\ <= '0';
        \_5\ <= clear;
        \_7\ <= reset;
        \_9\ <= clock;
        \_11\ <= bar;
        \_13\ <= foo;
        \_19\ <= (unsigned'("" & \_13\) + unsigned'("" & \_11\)) ?= "1";
        process (\_9\, \_7\) begin
            if rising_edge(\_7\) then
                \_33\ <= \_32\;
            else
                if rising_edge(\_9\) then
                    if \_5\ = '1' then
                        \_33\ <= \_32\;
                    else
                        if noo = '1' then
                            \_33\ <= \_19\;
                        end if;
                    end if;
                end if;
            end if;
        end process;
        aaa <= bbb;
        eee <= fff;
        ddd <= fff;
        ccc <= fff;
        coo <= noo;
        zoo <= \_33\;
        moo <= \_2\;
        sum <= \_18\;

    end architecture;
    |}]
;;
