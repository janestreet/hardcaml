open Hardcaml

let%expect_test "example" =
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
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"temp" (fun (i : _ I.t) ->
      let open Signal.Unoptimized in
      let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear ~reset:i.reset () in
      let zoo = i.foo +: i.bar in
      let coo = i.foo -: ~:(i.bar) -- "coo" in
      let doo = zoo |: ~:(coo -- "noo") -- "zoo" in
      let moo = mux2 zoo (doo @: doo) (zoo @: coo) in
      let moo =
        Signal.wireof
          ((moo |: ones 2 -- "aaa" -- "bbb" |: ones 2) -- "ccc" -- "ddd" -- "eee" -- "fff")
      in
      let zoo = Signal.reg spec ~enable:coo zoo in
      { zoo; moo })
  in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module temp (
        clear,
        reset,
        clock,
        bar,
        foo,
        zoo,
        moo
    );

        input clear;
        input reset;
        input clock;
        input bar;
        input foo;
        output zoo;
        output [1:0] moo;

        wire [1:0] _24;
        wire [1:0] bbb;
        wire [1:0] aaa;
        wire zoo_0;
        wire _19;
        wire [1:0] _20;
        wire [1:0] _17;
        wire [1:0] _21;
        wire [1:0] _23;
        wire [1:0] fff;
        wire [1:0] eee;
        wire [1:0] ddd;
        wire [1:0] ccc;
        wire [1:0] _1;
        wire _15;
        wire noo;
        wire coo;
        wire _27;
        wire _4;
        wire _6;
        wire _8;
        wire _10;
        wire _12;
        wire _14;
        reg _28;
        assign _24 = 2'b11;
        assign bbb = 2'b11;
        assign zoo_0 = ~ noo;
        assign _19 = _14 | zoo_0;
        assign _20 = { _19,
                       _19 };
        assign _17 = { _14,
                       noo };
        assign _21 = _14 ? _20 : _17;
        assign _23 = _21 | bbb;
        assign fff = _23 | _24;
        assign _1 = fff;
        assign _15 = ~ _10;
        assign noo = _12 - _15;
        assign _27 = 1'b0;
        assign _4 = clear;
        assign _6 = reset;
        assign _8 = clock;
        assign _10 = bar;
        assign _12 = foo;
        assign _14 = _12 + _10;
        always @(posedge _8 or posedge _6) begin
            if (_6)
                _28 <= _27;
            else
                if (_4)
                    _28 <= _27;
                else
                    if (noo)
                        _28 <= _14;
        end
        assign aaa = bbb;
        assign eee = fff;
        assign ddd = fff;
        assign ccc = fff;
        assign coo = noo;
        assign zoo = _28;
        assign moo = _1;

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
            moo : out std_logic_vector(1 downto 0)
        );
    end entity;

    architecture rtl of temp is

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
        signal hc_24 : std_logic_vector(1 downto 0);
        signal bbb : std_logic_vector(1 downto 0);
        signal aaa : std_logic_vector(1 downto 0);
        signal zoo_0 : std_logic;
        signal hc_19 : std_logic;
        signal hc_20 : std_logic_vector(1 downto 0);
        signal hc_17 : std_logic_vector(1 downto 0);
        signal hc_21 : std_logic_vector(1 downto 0);
        signal hc_23 : std_logic_vector(1 downto 0);
        signal fff : std_logic_vector(1 downto 0);
        signal eee : std_logic_vector(1 downto 0);
        signal ddd : std_logic_vector(1 downto 0);
        signal ccc : std_logic_vector(1 downto 0);
        signal hc_1 : std_logic_vector(1 downto 0);
        signal hc_15 : std_logic;
        signal noo : std_logic;
        signal coo : std_logic;
        signal hc_27 : std_logic;
        signal hc_4 : std_logic;
        signal hc_6 : std_logic;
        signal hc_8 : std_logic;
        signal hc_10 : std_logic;
        signal hc_12 : std_logic;
        signal hc_14 : std_logic;
        signal hc_28 : std_logic;

    begin

        hc_24 <= "11";
        bbb <= "11";
        zoo_0 <= hc_sl(not hc_uns(noo));
        hc_19 <= hc_sl(hc_uns(hc_14) or hc_uns(zoo_0));
        hc_20 <= hc_19 & hc_19;
        hc_17 <= hc_14 & noo;
        with to_integer(hc_uns(hc_14)) select hc_21 <=
            hc_17 when 0,
            hc_20 when others;
        hc_23 <= hc_slv(hc_uns(hc_21) or hc_uns(bbb));
        fff <= hc_slv(hc_uns(hc_23) or hc_uns(hc_24));
        hc_1 <= fff;
        hc_15 <= hc_sl(not hc_uns(hc_10));
        noo <= hc_sl(hc_uns(hc_12) - hc_uns(hc_15));
        hc_27 <= '0';
        hc_4 <= clear;
        hc_6 <= reset;
        hc_8 <= clock;
        hc_10 <= bar;
        hc_12 <= foo;
        hc_14 <= hc_sl(hc_uns(hc_12) + hc_uns(hc_10));
        process (hc_8, hc_6) begin
            if rising_edge(hc_6) then
                hc_28 <= hc_27;
            else
                if rising_edge(hc_8) then
                    if hc_4 = '1' then
                        hc_28 <= hc_27;
                    else
                        if noo = '1' then
                            hc_28 <= hc_14;
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
        zoo <= hc_28;
        moo <= hc_1;

    end architecture;
    |}]
;;
