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
  ; o_mux = mux i.i_a.:[2, 1] [ i.i_a; i.i_b; i.i_c.:[7, 0]; of_int_trunc ~width:8 0xab ]
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

        signal \_25\ : std_logic_vector(7 downto 0);
        signal \_24\ : std_logic_vector(7 downto 0);
        signal \_23\ : std_logic_vector(1 downto 0);
        signal \_26\ : std_logic_vector(7 downto 0);
        signal \_27\ : std_logic;
        signal \_28\ : std_logic_vector(7 downto 0);
        signal \_29\ : std_logic_vector(25 downto 0);
        signal \_30\ : std_logic_vector(15 downto 0);
        signal \_31\ : std_logic_vector(5 downto 0);
        signal \_32\ : std_logic_vector(1 downto 0);
        signal \_33\ : std_logic;
        signal \_34\ : std_logic;
        signal \_35\ : std_logic_vector(17 downto 0);
        signal \_11\ : std_logic_vector(9 downto 0);
        signal \_36\ : std_logic_vector(17 downto 0);
        signal \_37\ : std_logic_vector(7 downto 0);
        signal \_38\ : std_logic_vector(7 downto 0);
        signal \_39\ : std_logic_vector(7 downto 0);
        signal \a.b\ : std_logic_vector(7 downto 0);
        signal module : std_logic_vector(7 downto 0);
        signal \_19\ : std_logic_vector(7 downto 0);
        signal \_21\ : std_logic_vector(7 downto 0);
        signal \_42\ : std_logic_vector(7 downto 0);

    begin

        \_25\ <= "10101011";
        \_24\ <= \_11\(7 downto 0);
        \_23\ <= \_21\(2 downto 1);
        with to_integer(unsigned(\_23\)) select \_26\ <=
            \_21\ when 0,
            \_19\ when 1,
            \_24\ when 2,
            \_25\ when others;
        \_27\ <= \_21\(0);
        with to_integer(unsigned(std_logic_vector'("" & \_27\))) select \_28\ <=
            \_19\ when 0,
            \_21\ when others;
        \_29\ <= \_21\ & \_19\ & \_11\;
        \_30\ <= \_21\ & \_19\;
        \_31\ <= \_11\(9 downto 4);
        \_32\ <= \_21\(4 downto 3);
        \_33\ <= unsigned(\_21\) ?< unsigned(\_19\);
        \_34\ <= unsigned(\_21\) ?= unsigned(\_19\);
        \_35\ <= std_logic_vector(signed(\_21\) * signed(\_11\));
        \_11\ <= i_c;
        \_36\ <= std_logic_vector(unsigned(\_21\) * unsigned(\_11\));
        \_37\ <= std_logic_vector(unsigned(\_21\) - unsigned(\_19\));
        \_38\ <= std_logic_vector(unsigned(\_21\) + unsigned(\_19\));
        \_39\ <= not \_21\;
        \a.b\ <= \_21\ xor \_19\;
        module <= \_21\ or \_19\;
        \_19\ <= i_b;
        \_21\ <= i_a;
        \_42\ <= \_21\ and \_19\;
        o_and <= \_42\;
        o_or <= module;
        o_xor <= \a.b\;
        o_not <= \_39\;
        o_add <= \_38\;
        o_sub <= \_37\;
        o_mul <= \_36\;
        o_smul <= \_35\;
        o_eq <= \_34\;
        o_lt <= \_33\;
        o_select1 <= \_32\;
        o_select2 <= \_31\;
        o_cat1 <= \_30\;
        o_cat2 <= \_29\;
        o_mux2 <= \_28\;
        o_mux <= \_26\;

    end architecture;
    |}]
;;
