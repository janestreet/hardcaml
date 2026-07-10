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
        i_a,
        i_b,
        i_c,
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

        input [7:0] i_a;
        input [7:0] i_b;
        input [9:0] i_c;
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

        wire [7:0] signal_const;
        wire [7:0] signal_select;
        wire [1:0] signal_select_1;
        reg [7:0] signal_mux;
        wire signal_select_2;
        wire [7:0] signal_mux_1;
        wire [25:0] signal_cat;
        wire [15:0] signal_cat_1;
        wire [5:0] signal_select_3;
        wire [1:0] signal_select_4;
        wire signal_lt;
        wire signal_eq;
        wire [17:0] signal_muls;
        wire [9:0] signal_wire;
        wire [17:0] signal_mulu;
        wire [7:0] signal_sub;
        wire [7:0] signal_add;
        wire [7:0] signal_not;
        wire [7:0] \a.b ;
        wire [7:0] \module ;
        wire [7:0] signal_wire_1;
        wire [7:0] signal_wire_2;
        wire [7:0] signal_and;
        assign signal_const = 8'b10101011;
        assign signal_select = signal_wire[7:0];
        assign signal_select_1 = signal_wire_2[2:1];
        always @* begin
            case (signal_select_1)
            0:
                signal_mux <= signal_wire_2;
            1:
                signal_mux <= signal_wire_1;
            2:
                signal_mux <= signal_select;
            default:
                signal_mux <= signal_const;
            endcase
        end
        assign signal_select_2 = signal_wire_2[0:0];
        assign signal_mux_1 = signal_select_2 ? signal_wire_2 : signal_wire_1;
        assign signal_cat = { signal_wire_2,
                              signal_wire_1,
                              signal_wire };
        assign signal_cat_1 = { signal_wire_2,
                                signal_wire_1 };
        assign signal_select_3 = signal_wire[9:4];
        assign signal_select_4 = signal_wire_2[4:3];
        assign signal_lt = signal_wire_2 < signal_wire_1;
        assign signal_eq = signal_wire_2 == signal_wire_1;
        assign signal_muls = $signed(signal_wire_2) * $signed(signal_wire);
        assign signal_wire = i_c;
        assign signal_mulu = signal_wire_2 * signal_wire;
        assign signal_sub = signal_wire_2 - signal_wire_1;
        assign signal_add = signal_wire_2 + signal_wire_1;
        assign signal_not = ~ signal_wire_2;
        assign \a.b  = signal_wire_2 ^ signal_wire_1;
        assign \module  = signal_wire_2 | signal_wire_1;
        assign signal_wire_1 = i_b;
        assign signal_wire_2 = i_a;
        assign signal_and = signal_wire_2 & signal_wire_1;
        assign o_and = signal_and;
        assign o_or = \module ;
        assign o_xor = \a.b ;
        assign o_not = signal_not;
        assign o_add = signal_add;
        assign o_sub = signal_sub;
        assign o_mul = signal_mulu;
        assign o_smul = signal_muls;
        assign o_eq = signal_eq;
        assign o_lt = signal_lt;
        assign o_select1 = signal_select_4;
        assign o_select2 = signal_select_3;
        assign o_cat1 = signal_cat_1;
        assign o_cat2 = signal_cat;
        assign o_mux2 = signal_mux_1;
        assign o_mux = signal_mux;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity operators is
        port (
            i_a : in std_logic_vector(7 downto 0);
            i_b : in std_logic_vector(7 downto 0);
            i_c : in std_logic_vector(9 downto 0);
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

        signal signal_const : std_logic_vector(7 downto 0);
        signal signal_select : std_logic_vector(7 downto 0);
        signal signal_select_1 : std_logic_vector(1 downto 0);
        signal signal_mux : std_logic_vector(7 downto 0);
        signal signal_select_2 : std_logic;
        signal signal_mux_1 : std_logic_vector(7 downto 0);
        signal signal_cat : std_logic_vector(25 downto 0);
        signal signal_cat_1 : std_logic_vector(15 downto 0);
        signal signal_select_3 : std_logic_vector(5 downto 0);
        signal signal_select_4 : std_logic_vector(1 downto 0);
        signal signal_lt : std_logic;
        signal signal_eq : std_logic;
        signal signal_muls : std_logic_vector(17 downto 0);
        signal signal_wire : std_logic_vector(9 downto 0);
        signal signal_mulu : std_logic_vector(17 downto 0);
        signal signal_sub : std_logic_vector(7 downto 0);
        signal signal_add : std_logic_vector(7 downto 0);
        signal signal_not : std_logic_vector(7 downto 0);
        signal \a.b\ : std_logic_vector(7 downto 0);
        signal module : std_logic_vector(7 downto 0);
        signal signal_wire_1 : std_logic_vector(7 downto 0);
        signal signal_wire_2 : std_logic_vector(7 downto 0);
        signal signal_and : std_logic_vector(7 downto 0);

    begin

        signal_const <= "10101011";
        signal_select <= signal_wire(7 downto 0);
        signal_select_1 <= signal_wire_2(2 downto 1);
        with to_integer(unsigned(signal_select_1)) select signal_mux <=
            signal_wire_2 when 0,
            signal_wire_1 when 1,
            signal_select when 2,
            signal_const when others;
        signal_select_2 <= signal_wire_2(0);
        with to_integer(unsigned(std_logic_vector'("" & signal_select_2))) select signal_mux_1 <=
            signal_wire_1 when 0,
            signal_wire_2 when others;
        signal_cat <= signal_wire_2 & signal_wire_1 & signal_wire;
        signal_cat_1 <= signal_wire_2 & signal_wire_1;
        signal_select_3 <= signal_wire(9 downto 4);
        signal_select_4 <= signal_wire_2(4 downto 3);
        signal_lt <= unsigned(signal_wire_2) ?< unsigned(signal_wire_1);
        signal_eq <= unsigned(signal_wire_2) ?= unsigned(signal_wire_1);
        signal_muls <= std_logic_vector(signed(signal_wire_2) * signed(signal_wire));
        signal_wire <= i_c;
        signal_mulu <= std_logic_vector(unsigned(signal_wire_2) * unsigned(signal_wire));
        signal_sub <= std_logic_vector(unsigned(signal_wire_2) - unsigned(signal_wire_1));
        signal_add <= std_logic_vector(unsigned(signal_wire_2) + unsigned(signal_wire_1));
        signal_not <= not signal_wire_2;
        \a.b\ <= signal_wire_2 xor signal_wire_1;
        module <= signal_wire_2 or signal_wire_1;
        signal_wire_1 <= i_b;
        signal_wire_2 <= i_a;
        signal_and <= signal_wire_2 and signal_wire_1;
        o_and <= signal_and;
        o_or <= module;
        o_xor <= \a.b\;
        o_not <= signal_not;
        o_add <= signal_add;
        o_sub <= signal_sub;
        o_mul <= signal_mulu;
        o_smul <= signal_muls;
        o_eq <= signal_eq;
        o_lt <= signal_lt;
        o_select1 <= signal_select_4;
        o_select2 <= signal_select_3;
        o_cat1 <= signal_cat_1;
        o_cat2 <= signal_cat;
        o_mux2 <= signal_mux_1;
        o_mux <= signal_mux;

    end architecture;
    |}]
;;
