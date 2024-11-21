open! Core
open Hardcaml
open Signal
open Always

(* A simple statemachine used to test inference in vivado. *)

module State = struct
  type t =
    | A
    | B
    | C
    | D
  [@@deriving compare, enumerate, sexp_of]
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { is_done : 'a } [@@deriving hardcaml]
end

let create ?attribute ?encoding (i : _ I.t) =
  let attributes =
    Option.map attribute ~f:(fun a -> [ Rtl_attribute.Vivado.fsm_encoding a ])
  in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create ?attributes ?encoding (module State) spec in
  let cnt = Variable.reg spec ~width:4 in
  compile
    [ sm.switch
        [ A, [ when_ i.start [ sm.set_next B ] ]
        ; B, [ cnt <--. 0; sm.set_next C ]
        ; C, [ cnt <-- cnt.value +:. 1; when_ (cnt.value ==+. -1) [ sm.set_next D ] ]
        ; D, [ sm.set_next A ]
        ]
    ];
  { O.is_done = sm.is A }
;;

let print ?attribute ?encoding name lang =
  let module Circuit = Circuit.With_interface (I) (O) in
  let circuit = Circuit.create_exn ~name (create ?attribute ?encoding) in
  Rtl.print lang circuit
;;

let%expect_test "" =
  print "statemachine" Verilog;
  [%expect
    {|
    module statemachine (
        clear,
        clock,
        start,
        is_done
    );

        input clear;
        input clock;
        input start;
        output is_done;

        wire [1:0] _10;
        wire [1:0] _24;
        wire [3:0] _22;
        wire [3:0] _14;
        wire _2;
        wire _4;
        wire [3:0] _16;
        wire [3:0] _17;
        reg [3:0] _19;
        wire [3:0] _5;
        reg [3:0] _15;
        wire _23;
        wire [1:0] _25;
        wire [1:0] _18;
        wire [1:0] _13;
        wire _7;
        wire [1:0] _20;
        reg [1:0] _26;
        wire [1:0] _8;
        (* fsm_encoding="one_hot" *)
        reg [1:0] _11;
        wire _27;
        assign _10 = 2'b00;
        assign _24 = 2'b11;
        assign _22 = 4'b1111;
        assign _14 = 4'b0000;
        assign _2 = clear;
        assign _4 = clock;
        assign _16 = 4'b0001;
        assign _17 = _15 + _16;
        always @* begin
            case (_11)
            2'b01:
                _19 <= _14;
            2'b10:
                _19 <= _17;
            default:
                _19 <= _15;
            endcase
        end
        assign _5 = _19;
        always @(posedge _4) begin
            if (_2)
                _15 <= _14;
            else
                _15 <= _5;
        end
        assign _23 = _15 == _22;
        assign _25 = _23 ? _24 : _11;
        assign _18 = 2'b10;
        assign _13 = 2'b01;
        assign _7 = start;
        assign _20 = _7 ? _13 : _11;
        always @* begin
            case (_11)
            2'b00:
                _26 <= _20;
            2'b01:
                _26 <= _18;
            2'b10:
                _26 <= _25;
            2'b11:
                _26 <= _10;
            default:
                _26 <= _11;
            endcase
        end
        assign _8 = _26;
        always @(posedge _4) begin
            if (_2)
                _11 <= _10;
            else
                _11 <= _8;
        end
        assign _27 = _10 == _11;
        assign is_done = _27;

    endmodule
    |}];
  print "statemachine" Vhdl;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity statemachine is
        port (
            clear : in std_logic;
            clock : in std_logic;
            start : in std_logic;
            is_done : out std_logic
        );
    end entity;

    architecture rtl of statemachine is

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
        signal hc_10 : std_logic_vector(1 downto 0);
        signal hc_24 : std_logic_vector(1 downto 0);
        signal hc_22 : std_logic_vector(3 downto 0);
        signal hc_14 : std_logic_vector(3 downto 0);
        signal hc_2 : std_logic;
        signal hc_4 : std_logic;
        signal hc_16 : std_logic_vector(3 downto 0);
        signal hc_17 : std_logic_vector(3 downto 0);
        signal hc_19 : std_logic_vector(3 downto 0);
        signal hc_5 : std_logic_vector(3 downto 0);
        signal hc_15 : std_logic_vector(3 downto 0);
        signal hc_23 : std_logic;
        signal hc_25 : std_logic_vector(1 downto 0);
        signal hc_18 : std_logic_vector(1 downto 0);
        signal hc_13 : std_logic_vector(1 downto 0);
        signal hc_7 : std_logic;
        signal hc_20 : std_logic_vector(1 downto 0);
        signal hc_26 : std_logic_vector(1 downto 0);
        signal hc_8 : std_logic_vector(1 downto 0);
        signal hc_11 : std_logic_vector(1 downto 0);
        signal hc_27 : std_logic;

    begin

        hc_10 <= "00";
        hc_24 <= "11";
        hc_22 <= "1111";
        hc_14 <= "0000";
        hc_2 <= clear;
        hc_4 <= clock;
        hc_16 <= "0001";
        hc_17 <= hc_slv(hc_uns(hc_15) + hc_uns(hc_16));
        process (all) begin
            case hc_11 is
            when "01" =>
                hc_19 <= hc_14;
            when "10" =>
                hc_19 <= hc_17;
            when others =>
                hc_19 <= hc_15;
            end case;
        end process;
        hc_5 <= hc_19;
        process (hc_4) begin
            if rising_edge(hc_4) then
                if hc_2 = '1' then
                    hc_15 <= hc_14;
                else
                    hc_15 <= hc_5;
                end if;
            end if;
        end process;
        hc_23 <= hc_sl(hc_uns(hc_15) = hc_uns(hc_22));
        with to_integer(hc_uns(hc_23)) select hc_25 <=
            hc_11 when 0,
            hc_24 when others;
        hc_18 <= "10";
        hc_13 <= "01";
        hc_7 <= start;
        with to_integer(hc_uns(hc_7)) select hc_20 <=
            hc_11 when 0,
            hc_13 when others;
        process (all) begin
            case hc_11 is
            when "00" =>
                hc_26 <= hc_20;
            when "01" =>
                hc_26 <= hc_18;
            when "10" =>
                hc_26 <= hc_25;
            when "11" =>
                hc_26 <= hc_10;
            when others =>
                hc_26 <= hc_11;
            end case;
        end process;
        hc_8 <= hc_26;
        process (hc_4) begin
            if rising_edge(hc_4) then
                if hc_2 = '1' then
                    hc_11 <= hc_10;
                else
                    hc_11 <= hc_8;
                end if;
            end if;
        end process;
        hc_27 <= hc_sl(hc_uns(hc_10) = hc_uns(hc_11));
        is_done <= hc_27;

    end architecture;
    |}]
;;

let%expect_test "show that the pseudo-constants for matches are not printed seperately" =
  let q =
    cases
      ~default:(input "def" 8)
      (input "select" 32)
      (List.init 7 ~f:(fun i -> Signal.random ~width:32, input [%string "d%{i#Int}"] 8))
  in
  let circ = Circuit.create_exn ~name:"cases" [ output "q" q ] in
  Rtl.print Verilog circ;
  [%expect
    {|
    module cases (
        def,
        d6,
        d5,
        d4,
        d3,
        d2,
        d1,
        d0,
        select,
        q
    );

        input [7:0] def;
        input [7:0] d6;
        input [7:0] d5;
        input [7:0] d4;
        input [7:0] d3;
        input [7:0] d2;
        input [7:0] d1;
        input [7:0] d0;
        input [31:0] select;
        output [7:0] q;

        reg [7:0] _18;
        always @* begin
            case (select)
            32'b11101111101011000111100110100000:
                _18 <= d0;
            32'b01001001100011001101101000110110:
                _18 <= d1;
            32'b00000101111010001100110101100001:
                _18 <= d2;
            32'b10001100111011010010011011111100:
                _18 <= d3;
            32'b11100010000011110010101010010011:
                _18 <= d4;
            32'b01001110100011101100011101100101:
                _18 <= d5;
            32'b10011000111110001101010011000011:
                _18 <= d6;
            default:
                _18 <= def;
            endcase
        end
        assign q = _18;

    endmodule
    |}]
;;
