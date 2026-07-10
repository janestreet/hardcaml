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
        clock,
        clear,
        reset,
        foo,
        bar,
        zoo,
        moo,
        sum
    );

        input clock;
        input clear;
        input reset;
        input foo;
        input bar;
        output zoo;
        output [1:0] moo;
        output [1:0] sum;

        wire [1:0] signal_cat;
        wire gnd;
        wire [1:0] signal_cat_1;
        wire [1:0] signal_add;
        wire [1:0] signal_const;
        wire [1:0] bbb;
        wire [1:0] aaa;
        wire zoo_0;
        wire signal_or;
        wire [1:0] signal_cat_2;
        wire [1:0] signal_cat_3;
        wire [1:0] signal_mux;
        wire [1:0] signal_or_1;
        wire [1:0] fff;
        wire [1:0] eee;
        wire [1:0] ddd;
        wire [1:0] ccc;
        wire [1:0] signal_wire;
        wire signal_not;
        wire noo;
        wire coo;
        wire signal_const_1;
        wire signal_wire_1;
        wire signal_wire_2;
        wire signal_wire_3;
        wire signal_wire_4;
        wire signal_wire_5;
        wire signal_add_1;
        reg signal_reg;
        assign signal_cat = { gnd,
                              signal_wire_4 };
        assign gnd = 1'b0;
        assign signal_cat_1 = { gnd,
                                signal_wire_5 };
        assign signal_add = signal_cat_1 + signal_cat;
        assign signal_const = 2'b11;
        assign bbb = 2'b11;
        assign zoo_0 = ~ noo;
        assign signal_or = signal_add_1 | zoo_0;
        assign signal_cat_2 = { signal_or,
                                signal_or };
        assign signal_cat_3 = { signal_add_1,
                                noo };
        assign signal_mux = signal_add_1 ? signal_cat_2 : signal_cat_3;
        assign signal_or_1 = signal_mux | bbb;
        assign fff = signal_or_1 | signal_const;
        assign signal_wire = fff;
        assign signal_not = ~ signal_wire_4;
        assign noo = signal_wire_5 - signal_not;
        assign signal_const_1 = 1'b0;
        assign signal_wire_1 = clear;
        assign signal_wire_2 = reset;
        assign signal_wire_3 = clock;
        assign signal_wire_4 = bar;
        assign signal_wire_5 = foo;
        assign signal_add_1 = signal_wire_5 + signal_wire_4;
        always @(posedge signal_wire_3 or posedge signal_wire_2) begin
            if (signal_wire_2)
                signal_reg <= signal_const_1;
            else
                if (signal_wire_1)
                    signal_reg <= signal_const_1;
                else
                    if (noo)
                        signal_reg <= signal_add_1;
        end
        assign aaa = bbb;
        assign eee = fff;
        assign ddd = fff;
        assign ccc = fff;
        assign coo = noo;
        assign zoo = signal_reg;
        assign moo = signal_wire;
        assign sum = signal_add;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity temp is
        port (
            clock : in std_logic;
            clear : in std_logic;
            reset : in std_logic;
            foo : in std_logic;
            bar : in std_logic;
            zoo : out std_logic;
            moo : out std_logic_vector(1 downto 0);
            sum : out std_logic_vector(1 downto 0)
        );
    end entity;

    architecture rtl of temp is

        signal signal_cat : std_logic_vector(1 downto 0);
        signal gnd : std_logic;
        signal signal_cat_1 : std_logic_vector(1 downto 0);
        signal signal_add : std_logic_vector(1 downto 0);
        signal signal_const : std_logic_vector(1 downto 0);
        signal bbb : std_logic_vector(1 downto 0);
        signal aaa : std_logic_vector(1 downto 0);
        signal zoo_0 : std_logic;
        signal signal_or : std_logic;
        signal signal_cat_2 : std_logic_vector(1 downto 0);
        signal signal_cat_3 : std_logic_vector(1 downto 0);
        signal signal_mux : std_logic_vector(1 downto 0);
        signal signal_or_1 : std_logic_vector(1 downto 0);
        signal fff : std_logic_vector(1 downto 0);
        signal eee : std_logic_vector(1 downto 0);
        signal ddd : std_logic_vector(1 downto 0);
        signal ccc : std_logic_vector(1 downto 0);
        signal signal_wire : std_logic_vector(1 downto 0);
        signal signal_not : std_logic;
        signal noo : std_logic;
        signal coo : std_logic;
        signal signal_const_1 : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_wire_2 : std_logic;
        signal signal_wire_3 : std_logic;
        signal signal_wire_4 : std_logic;
        signal signal_wire_5 : std_logic;
        signal signal_add_1 : std_logic;
        signal signal_reg : std_logic;

    begin

        signal_cat <= gnd & signal_wire_4;
        gnd <= '0';
        signal_cat_1 <= gnd & signal_wire_5;
        signal_add <= std_logic_vector(unsigned(signal_cat_1) + unsigned(signal_cat));
        signal_const <= "11";
        bbb <= "11";
        zoo_0 <= not noo;
        signal_or <= signal_add_1 or zoo_0;
        signal_cat_2 <= signal_or & signal_or;
        signal_cat_3 <= signal_add_1 & noo;
        with to_integer(unsigned(std_logic_vector'("" & signal_add_1))) select signal_mux <=
            signal_cat_3 when 0,
            signal_cat_2 when others;
        signal_or_1 <= signal_mux or bbb;
        fff <= signal_or_1 or signal_const;
        signal_wire <= fff;
        signal_not <= not signal_wire_4;
        noo <= (unsigned(std_logic_vector'("" & signal_wire_5)) - unsigned(std_logic_vector'("" & signal_not))) ?= "1";
        signal_const_1 <= '0';
        signal_wire_1 <= clear;
        signal_wire_2 <= reset;
        signal_wire_3 <= clock;
        signal_wire_4 <= bar;
        signal_wire_5 <= foo;
        signal_add_1 <= (unsigned(std_logic_vector'("" & signal_wire_5)) + unsigned(std_logic_vector'("" & signal_wire_4))) ?= "1";
        process (signal_wire_3, signal_wire_2) begin
            if signal_wire_2 = '1' then
                signal_reg <= signal_const_1;
            else
                if rising_edge(signal_wire_3) then
                    if signal_wire_1 = '1' then
                        signal_reg <= signal_const_1;
                    else
                        if noo = '1' then
                            signal_reg <= signal_add_1;
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
        zoo <= signal_reg;
        moo <= signal_wire;
        sum <= signal_add;

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
        clock,
        clear,
        reset,
        foo,
        bar,
        zoo,
        moo,
        sum
    );

        input clock;
        input clear;
        input reset;
        input foo;
        input bar;
        output zoo;
        output [1:0] moo;
        output [1:0] sum;

        bit [1:0] signal_cat;
        bit gnd;
        bit [1:0] signal_cat_1;
        bit [1:0] signal_add;
        bit [1:0] signal_const;
        bit [1:0] bbb;
        bit [1:0] aaa;
        bit zoo_0;
        bit signal_or;
        bit [1:0] signal_cat_2;
        bit [1:0] signal_cat_3;
        bit [1:0] signal_mux;
        bit [1:0] signal_or_1;
        bit [1:0] fff;
        bit [1:0] eee;
        bit [1:0] ddd;
        bit [1:0] ccc;
        bit [1:0] signal_wire;
        bit signal_not;
        bit noo;
        bit coo;
        bit signal_const_1;
        bit signal_wire_1;
        bit signal_wire_2;
        bit signal_wire_3;
        bit signal_wire_4;
        bit signal_wire_5;
        bit signal_add_1;
        bit signal_reg;
        assign signal_cat = { gnd,
                              signal_wire_4 };
        assign gnd = 1'b0;
        assign signal_cat_1 = { gnd,
                                signal_wire_5 };
        assign signal_add = signal_cat_1 + signal_cat;
        assign signal_const = 2'b11;
        assign bbb = 2'b11;
        assign zoo_0 = ~ noo;
        assign signal_or = signal_add_1 | zoo_0;
        assign signal_cat_2 = { signal_or,
                                signal_or };
        assign signal_cat_3 = { signal_add_1,
                                noo };
        assign signal_mux = signal_add_1 ? signal_cat_2 : signal_cat_3;
        assign signal_or_1 = signal_mux | bbb;
        assign fff = signal_or_1 | signal_const;
        assign signal_wire = fff;
        assign signal_not = ~ signal_wire_4;
        assign noo = signal_wire_5 - signal_not;
        assign signal_const_1 = 1'b0;
        assign signal_wire_1 = clear;
        assign signal_wire_2 = reset;
        assign signal_wire_3 = clock;
        assign signal_wire_4 = bar;
        assign signal_wire_5 = foo;
        assign signal_add_1 = signal_wire_5 + signal_wire_4;
        always @(posedge signal_wire_3 or posedge signal_wire_2) begin
            if (signal_wire_2)
                signal_reg <= signal_const_1;
            else
                if (signal_wire_1)
                    signal_reg <= signal_const_1;
                else
                    if (noo)
                        signal_reg <= signal_add_1;
        end
        assign aaa = bbb;
        assign eee = fff;
        assign ddd = fff;
        assign ccc = fff;
        assign coo = noo;
        assign zoo = signal_reg;
        assign moo = signal_wire;
        assign sum = signal_add;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_bit.all;

    entity temp is
        port (
            clock : in bit;
            clear : in bit;
            reset : in bit;
            foo : in bit;
            bar : in bit;
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

        signal signal_cat : bit_vector(1 downto 0);
        signal gnd : bit;
        signal signal_cat_1 : bit_vector(1 downto 0);
        signal signal_add : bit_vector(1 downto 0);
        signal signal_const : bit_vector(1 downto 0);
        signal bbb : bit_vector(1 downto 0);
        signal aaa : bit_vector(1 downto 0);
        signal zoo_0 : bit;
        signal signal_or : bit;
        signal signal_cat_2 : bit_vector(1 downto 0);
        signal signal_cat_3 : bit_vector(1 downto 0);
        signal signal_mux : bit_vector(1 downto 0);
        signal signal_or_1 : bit_vector(1 downto 0);
        signal fff : bit_vector(1 downto 0);
        signal eee : bit_vector(1 downto 0);
        signal ddd : bit_vector(1 downto 0);
        signal ccc : bit_vector(1 downto 0);
        signal signal_wire : bit_vector(1 downto 0);
        signal signal_not : bit;
        signal noo : bit;
        signal coo : bit;
        signal signal_const_1 : bit;
        signal signal_wire_1 : bit;
        signal signal_wire_2 : bit;
        signal signal_wire_3 : bit;
        signal signal_wire_4 : bit;
        signal signal_wire_5 : bit;
        signal signal_add_1 : bit;
        signal signal_reg : bit;

    begin

        signal_cat <= gnd & signal_wire_4;
        gnd <= '0';
        signal_cat_1 <= gnd & signal_wire_5;
        signal_add <= bit_vector(unsigned(signal_cat_1) + unsigned(signal_cat));
        signal_const <= "11";
        bbb <= "11";
        zoo_0 <= not noo;
        signal_or <= signal_add_1 or zoo_0;
        signal_cat_2 <= signal_or & signal_or;
        signal_cat_3 <= signal_add_1 & noo;
        with to_integer(unsigned'("" & signal_add_1)) select signal_mux <=
            signal_cat_3 when 0,
            signal_cat_2 when others;
        signal_or_1 <= signal_mux or bbb;
        fff <= signal_or_1 or signal_const;
        signal_wire <= fff;
        signal_not <= not signal_wire_4;
        noo <= (unsigned'("" & signal_wire_5) - unsigned'("" & signal_not)) ?= "1";
        signal_const_1 <= '0';
        signal_wire_1 <= clear;
        signal_wire_2 <= reset;
        signal_wire_3 <= clock;
        signal_wire_4 <= bar;
        signal_wire_5 <= foo;
        signal_add_1 <= (unsigned'("" & signal_wire_5) + unsigned'("" & signal_wire_4)) ?= "1";
        process (signal_wire_3, signal_wire_2) begin
            if signal_wire_2 = '1' then
                signal_reg <= signal_const_1;
            else
                if rising_edge(signal_wire_3) then
                    if signal_wire_1 = '1' then
                        signal_reg <= signal_const_1;
                    else
                        if noo = '1' then
                            signal_reg <= signal_add_1;
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
        zoo <= signal_reg;
        moo <= signal_wire;
        sum <= signal_add;

    end architecture;
    |}]
;;
