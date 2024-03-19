open Core
open Hardcaml

module I = struct
  type 'a t =
    { i_a : 'a [@bits 8]
    ; i_b : 'a [@bits 8]
    ; i_c : 'a [@bits 10]
    }
  [@@deriving sexp_of, hardcaml]
end

module O = struct
  type 'a t =
    { o_and : 'a [@bits 8]
    ; o_or : 'a [@bits 8]
    ; o_xor : 'a [@bits 8]
    ; o_not : 'a [@bits 8]
    ; o_add : 'a [@bits 8]
    ; o_sub : 'a [@bits 8]
    ; o_mul : 'a [@bits 18]
    ; o_smul : 'a [@bits 18]
    ; o_eq : 'a
    ; o_lt : 'a
    ; o_select1 : 'a [@bits 2]
    ; o_select2 : 'a [@bits 6]
    ; o_cat1 : 'a [@bits 16]
    ; o_cat2 : 'a [@bits 26]
    ; o_mux2 : 'a [@bits 8]
    ; o_mux : 'a [@bits 8]
    }
  [@@deriving sexp_of, hardcaml]
end

let create (i : _ I.t) =
  let open Signal in
  { O.o_and = i.i_a &: i.i_b
  ; o_or = (i.i_a |: i.i_b) -- "module"
  ; o_xor = (i.i_a ^: i.i_b) -- "a.b"
  ; o_not = ~:(i.i_a)
  ; o_add = i.i_a +: i.i_b
  ; o_sub = i.i_a -: i.i_b
  ; o_mul = i.i_a *: i.i_c
  ; o_smul = i.i_a *+ i.i_c
  ; o_eq = i.i_a ==: i.i_b
  ; o_lt = i.i_a <: i.i_b
  ; o_select1 = i.i_a.:[4, 3]
  ; o_select2 = i.i_c.:[9, 4]
  ; o_cat1 = i.i_a @: i.i_b
  ; o_cat2 = concat_msb [ i.i_a; i.i_b; i.i_c ]
  ; o_mux2 = mux2 i.i_a.:[0, 0] i.i_a i.i_b
  ; o_mux = mux i.i_a.:[2, 1] [ i.i_a; i.i_b; i.i_c.:[7, 0]; of_int ~width:8 0xab ]
  }
;;

module C = Circuit.With_interface (I) (O)

let%expect_test "operators" =
  let circuit = C.create_exn ~name:"operators" create in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module operators (
        i_c,
        i_b,
        i_a,
        o_and,
        o_or,
        o_xor,
        o_not,
        o_add,
        o_sub,
        o_mul,
        o_smul,
        o_eq,
        o_lt,
        o_select1,
        o_select2,
        o_cat1,
        o_cat2,
        o_mux2,
        o_mux
    );

        input [9:0] i_c;
        input [7:0] i_b;
        input [7:0] i_a;
        output [7:0] o_and;
        output [7:0] o_or;
        output [7:0] o_xor;
        output [7:0] o_not;
        output [7:0] o_add;
        output [7:0] o_sub;
        output [17:0] o_mul;
        output [17:0] o_smul;
        output o_eq;
        output o_lt;
        output [1:0] o_select1;
        output [5:0] o_select2;
        output [15:0] o_cat1;
        output [25:0] o_cat2;
        output [7:0] o_mux2;
        output [7:0] o_mux;

        wire [7:0] _25;
        wire [7:0] _24;
        wire [1:0] _23;
        reg [7:0] _26;
        wire _27;
        wire [7:0] _28;
        wire [25:0] _29;
        wire [15:0] _30;
        wire [5:0] _31;
        wire [1:0] _32;
        wire _33;
        wire _34;
        wire [17:0] _35;
        wire [9:0] _11;
        wire [17:0] _36;
        wire [7:0] _37;
        wire [7:0] _38;
        wire [7:0] _39;
        wire [7:0] a_b;
        wire [7:0] module_0;
        wire [7:0] _19;
        wire [7:0] _21;
        wire [7:0] _42;
        assign _25 = 8'b10101011;
        assign _24 = _11[7:0];
        assign _23 = _21[2:1];
        always @* begin
            case (_23)
            0:
                _26 <= _21;
            1:
                _26 <= _19;
            2:
                _26 <= _24;
            default:
                _26 <= _25;
            endcase
        end
        assign _27 = _21[0:0];
        assign _28 = _27 ? _21 : _19;
        assign _29 = { _21,
                       _19,
                       _11 };
        assign _30 = { _21,
                       _19 };
        assign _31 = _11[9:4];
        assign _32 = _21[4:3];
        assign _33 = _21 < _19;
        assign _34 = _21 == _19;
        assign _35 = $signed(_21) * $signed(_11);
        assign _11 = i_c;
        assign _36 = _21 * _11;
        assign _37 = _21 - _19;
        assign _38 = _21 + _19;
        assign _39 = ~ _21;
        assign a_b = _21 ^ _19;
        assign module_0 = _21 | _19;
        assign _19 = i_b;
        assign _21 = i_a;
        assign _42 = _21 & _19;
        assign o_and = _42;
        assign o_or = module_0;
        assign o_xor = a_b;
        assign o_not = _39;
        assign o_add = _38;
        assign o_sub = _37;
        assign o_mul = _36;
        assign o_smul = _35;
        assign o_eq = _34;
        assign o_lt = _33;
        assign o_select1 = _32;
        assign o_select2 = _31;
        assign o_cat1 = _30;
        assign o_cat2 = _29;
        assign o_mux2 = _28;
        assign o_mux = _26;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity operators is
        port (
            i_c : in std_logic_vector(9 downto 0);
            i_b : in std_logic_vector(7 downto 0);
            i_a : in std_logic_vector(7 downto 0);
            o_and : out std_logic_vector(7 downto 0);
            o_or : out std_logic_vector(7 downto 0);
            o_xor : out std_logic_vector(7 downto 0);
            o_not : out std_logic_vector(7 downto 0);
            o_add : out std_logic_vector(7 downto 0);
            o_sub : out std_logic_vector(7 downto 0);
            o_mul : out std_logic_vector(17 downto 0);
            o_smul : out std_logic_vector(17 downto 0);
            o_eq : out std_logic;
            o_lt : out std_logic;
            o_select1 : out std_logic_vector(1 downto 0);
            o_select2 : out std_logic_vector(5 downto 0);
            o_cat1 : out std_logic_vector(15 downto 0);
            o_cat2 : out std_logic_vector(25 downto 0);
            o_mux2 : out std_logic_vector(7 downto 0);
            o_mux : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of operators is

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
        signal hc_25 : std_logic_vector(7 downto 0);
        signal hc_24 : std_logic_vector(7 downto 0);
        signal hc_23 : std_logic_vector(1 downto 0);
        signal hc_26 : std_logic_vector(7 downto 0);
        signal hc_27 : std_logic;
        signal hc_28 : std_logic_vector(7 downto 0);
        signal hc_29 : std_logic_vector(25 downto 0);
        signal hc_30 : std_logic_vector(15 downto 0);
        signal hc_31 : std_logic_vector(5 downto 0);
        signal hc_32 : std_logic_vector(1 downto 0);
        signal hc_33 : std_logic;
        signal hc_34 : std_logic;
        signal hc_35 : std_logic_vector(17 downto 0);
        signal hc_11 : std_logic_vector(9 downto 0);
        signal hc_36 : std_logic_vector(17 downto 0);
        signal hc_37 : std_logic_vector(7 downto 0);
        signal hc_38 : std_logic_vector(7 downto 0);
        signal hc_39 : std_logic_vector(7 downto 0);
        signal a_b : std_logic_vector(7 downto 0);
        signal module : std_logic_vector(7 downto 0);
        signal hc_19 : std_logic_vector(7 downto 0);
        signal hc_21 : std_logic_vector(7 downto 0);
        signal hc_42 : std_logic_vector(7 downto 0);

    begin

        hc_25 <= "10101011";
        hc_24 <= hc_11(7 downto 0);
        hc_23 <= hc_21(2 downto 1);
        with to_integer(hc_uns(hc_23)) select hc_26 <=
            hc_21 when 0,
            hc_19 when 1,
            hc_24 when 2,
            hc_25 when others;
        hc_27 <= hc_sl(hc_21(0 downto 0));
        with to_integer(hc_uns(hc_27)) select hc_28 <=
            hc_19 when 0,
            hc_21 when others;
        hc_29 <= hc_21 & hc_19 & hc_11;
        hc_30 <= hc_21 & hc_19;
        hc_31 <= hc_11(9 downto 4);
        hc_32 <= hc_21(4 downto 3);
        hc_33 <= hc_sl(hc_uns(hc_21) < hc_uns(hc_19));
        hc_34 <= hc_sl(hc_uns(hc_21) = hc_uns(hc_19));
        hc_35 <= hc_slv(hc_sgn(hc_21) * hc_sgn(hc_11));
        hc_11 <= i_c;
        hc_36 <= hc_slv(hc_uns(hc_21) * hc_uns(hc_11));
        hc_37 <= hc_slv(hc_uns(hc_21) - hc_uns(hc_19));
        hc_38 <= hc_slv(hc_uns(hc_21) + hc_uns(hc_19));
        hc_39 <= hc_slv(not hc_uns(hc_21));
        a_b <= hc_slv(hc_uns(hc_21) xor hc_uns(hc_19));
        module <= hc_slv(hc_uns(hc_21) or hc_uns(hc_19));
        hc_19 <= i_b;
        hc_21 <= i_a;
        hc_42 <= hc_slv(hc_uns(hc_21) and hc_uns(hc_19));
        o_and <= hc_42;
        o_or <= module;
        o_xor <= a_b;
        o_not <= hc_39;
        o_add <= hc_38;
        o_sub <= hc_37;
        o_mul <= hc_36;
        o_smul <= hc_35;
        o_eq <= hc_34;
        o_lt <= hc_33;
        o_select1 <= hc_32;
        o_select2 <= hc_31;
        o_cat1 <= hc_30;
        o_cat2 <= hc_29;
        o_mux2 <= hc_28;
        o_mux <= hc_26;

    end architecture;
    |}]
;;
