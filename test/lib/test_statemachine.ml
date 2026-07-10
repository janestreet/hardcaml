open! Core
open Expect_test_helpers_base
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
  [@@deriving compare ~localize, enumerate, sexp_of]
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
        clock,
        clear,
        start,
        is_done
    );

        input clock;
        input clear;
        input start;
        output is_done;

        wire [1:0] signal_const;
        wire [1:0] signal_const_1;
        wire [3:0] signal_const_2;
        wire [3:0] signal_const_3;
        wire signal_wire;
        wire signal_wire_1;
        wire [3:0] signal_const_4;
        wire [3:0] signal_add;
        reg [3:0] signal_cases;
        wire [3:0] signal_wire_2;
        reg [3:0] signal_reg;
        wire signal_eq;
        wire [1:0] signal_mux;
        wire [1:0] signal_const_6;
        wire [1:0] signal_const_7;
        wire signal_wire_3;
        wire [1:0] signal_mux_1;
        reg [1:0] signal_cases_1;
        wire [1:0] signal_wire_4;
        (* fsm_encoding="one_hot" *)
        reg [1:0] signal_reg_1;
        wire signal_eq_1;
        assign signal_const = 2'b00;
        assign signal_const_1 = 2'b11;
        assign signal_const_2 = 4'b1111;
        assign signal_const_3 = 4'b0000;
        assign signal_wire = clear;
        assign signal_wire_1 = clock;
        assign signal_const_4 = 4'b0001;
        assign signal_add = signal_reg + signal_const_4;
        always @* begin
            case (signal_reg_1)
            2'b01:
                signal_cases <= signal_const_3;
            2'b10:
                signal_cases <= signal_add;
            default:
                signal_cases <= signal_reg;
            endcase
        end
        assign signal_wire_2 = signal_cases;
        always @(posedge signal_wire_1) begin
            if (signal_wire)
                signal_reg <= signal_const_3;
            else
                signal_reg <= signal_wire_2;
        end
        assign signal_eq = signal_reg == signal_const_2;
        assign signal_mux = signal_eq ? signal_const_1 : signal_reg_1;
        assign signal_const_6 = 2'b10;
        assign signal_const_7 = 2'b01;
        assign signal_wire_3 = start;
        assign signal_mux_1 = signal_wire_3 ? signal_const_7 : signal_reg_1;
        always @* begin
            case (signal_reg_1)
            2'b00:
                signal_cases_1 <= signal_mux_1;
            2'b01:
                signal_cases_1 <= signal_const_6;
            2'b10:
                signal_cases_1 <= signal_mux;
            2'b11:
                signal_cases_1 <= signal_const;
            default:
                signal_cases_1 <= signal_reg_1;
            endcase
        end
        assign signal_wire_4 = signal_cases_1;
        always @(posedge signal_wire_1) begin
            if (signal_wire)
                signal_reg_1 <= signal_const;
            else
                signal_reg_1 <= signal_wire_4;
        end
        assign signal_eq_1 = signal_const == signal_reg_1;
        assign is_done = signal_eq_1;

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
            clock : in std_logic;
            clear : in std_logic;
            start : in std_logic;
            is_done : out std_logic
        );
        attribute fsm_encoding : string;
    end entity;

    architecture rtl of statemachine is

        signal signal_const : std_logic_vector(1 downto 0);
        signal signal_const_1 : std_logic_vector(1 downto 0);
        signal signal_const_2 : std_logic_vector(3 downto 0);
        signal signal_const_3 : std_logic_vector(3 downto 0);
        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_const_4 : std_logic_vector(3 downto 0);
        signal signal_add : std_logic_vector(3 downto 0);
        signal signal_cases : std_logic_vector(3 downto 0);
        signal signal_wire_2 : std_logic_vector(3 downto 0);
        signal signal_reg : std_logic_vector(3 downto 0);
        signal signal_eq : std_logic;
        signal signal_mux : std_logic_vector(1 downto 0);
        signal signal_const_6 : std_logic_vector(1 downto 0);
        signal signal_const_7 : std_logic_vector(1 downto 0);
        signal signal_wire_3 : std_logic;
        signal signal_mux_1 : std_logic_vector(1 downto 0);
        signal signal_cases_1 : std_logic_vector(1 downto 0);
        signal signal_wire_4 : std_logic_vector(1 downto 0);
        signal signal_reg_1 : std_logic_vector(1 downto 0);
        attribute fsm_encoding of signal_reg_1 : signal is "one_hot";
        signal signal_eq_1 : std_logic;

    begin

        signal_const <= "00";
        signal_const_1 <= "11";
        signal_const_2 <= "1111";
        signal_const_3 <= "0000";
        signal_wire <= clear;
        signal_wire_1 <= clock;
        signal_const_4 <= "0001";
        signal_add <= std_logic_vector(unsigned(signal_reg) + unsigned(signal_const_4));
        process (all) begin
            case signal_reg_1 is
            when "01" =>
                signal_cases <= signal_const_3;
            when "10" =>
                signal_cases <= signal_add;
            when others =>
                signal_cases <= signal_reg;
            end case;
        end process;
        signal_wire_2 <= signal_cases;
        process (signal_wire_1) begin
            if rising_edge(signal_wire_1) then
                if signal_wire = '1' then
                    signal_reg <= signal_const_3;
                else
                    signal_reg <= signal_wire_2;
                end if;
            end if;
        end process;
        signal_eq <= unsigned(signal_reg) ?= unsigned(signal_const_2);
        with to_integer(unsigned(std_logic_vector'("" & signal_eq))) select signal_mux <=
            signal_reg_1 when 0,
            signal_const_1 when others;
        signal_const_6 <= "10";
        signal_const_7 <= "01";
        signal_wire_3 <= start;
        with to_integer(unsigned(std_logic_vector'("" & signal_wire_3))) select signal_mux_1 <=
            signal_reg_1 when 0,
            signal_const_7 when others;
        process (all) begin
            case signal_reg_1 is
            when "00" =>
                signal_cases_1 <= signal_mux_1;
            when "01" =>
                signal_cases_1 <= signal_const_6;
            when "10" =>
                signal_cases_1 <= signal_mux;
            when "11" =>
                signal_cases_1 <= signal_const;
            when others =>
                signal_cases_1 <= signal_reg_1;
            end case;
        end process;
        signal_wire_4 <= signal_cases_1;
        process (signal_wire_1) begin
            if rising_edge(signal_wire_1) then
                if signal_wire = '1' then
                    signal_reg_1 <= signal_const;
                else
                    signal_reg_1 <= signal_wire_4;
                end if;
            end if;
        end process;
        signal_eq_1 <= unsigned(signal_const) ?= unsigned(signal_reg_1);
        is_done <= signal_eq_1;

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

        reg [7:0] signal_cases;
        always @* begin
            case (select)
            32'b11101111101011000111100110100000:
                signal_cases <= d0;
            32'b01001001100011001101101000110110:
                signal_cases <= d1;
            32'b00000101111010001100110101100001:
                signal_cases <= d2;
            32'b10001100111011010010011011111100:
                signal_cases <= d3;
            32'b11100010000011110010101010010011:
                signal_cases <= d4;
            32'b01001110100011101100011101100101:
                signal_cases <= d5;
            32'b10011000111110001101010011000011:
                signal_cases <= d6;
            default:
                signal_cases <= def;
            endcase
        end
        assign q = signal_cases;

    endmodule
    |}]
;;

let create ~unreachable cases =
  let i = I.Of_signal.wires () in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = State_machine.create ~unreachable (module State) spec in
  compile [ sm.switch (cases sm.set_next) ];
  { O.is_done = sm.is A }
;;

let%expect_test "unreachable" =
  require_does_not_raise (fun () ->
    ignore
      (create ~unreachable:[] (fun next ->
         [ A, [ next A ]; B, [ next B ]; C, [ next C ]; D, [ next D ] ])
       : _ O.t));
  require_does_not_raise (fun () ->
    ignore
      (create ~unreachable:[ B ] (fun next ->
         [ A, [ next A ]; C, [ next C ]; D, [ next D ] ])
       : _ O.t));
  require_does_not_raise (fun () ->
    ignore
      (create ~unreachable:[ B; D ] (fun next -> [ A, [ next A ]; C, [ next C ] ])
       : _ O.t));
  require_does_raise (fun () ->
    ignore
      (create ~unreachable:[ B; D ] (fun next ->
         [ A, [ next A ]; B, [ next A ]; C, [ next C ] ])
       : _ O.t));
  [%expect
    {|
    ("[Always.State_machine.switch] unreachable state provided with a non-empty implementation"
     (state B))
    |}];
  require_does_not_raise (fun () ->
    ignore
      (create ~unreachable:[ B; D ] (fun next -> [ A, [ next A ]; B, []; C, [ next C ] ])
       : _ O.t));
  [%expect {| |}];
  require_does_raise (fun () ->
    ignore
      (create ~unreachable:[ A; D ] (fun next -> [ A, []; B, []; C, [ next C ]; D, [] ])
       : _ O.t));
  [%expect {| ("[Always.State_machine.is] got unknown state" A) |}];
  require_does_raise (fun () ->
    ignore
      (create ~unreachable:[ B; D ] (fun next -> [ A, [ next A ]; C, [ next D ] ])
       : _ O.t));
  [%expect {| ("[Always.State_machine.set_next] got unknown state" D) |}]
;;
