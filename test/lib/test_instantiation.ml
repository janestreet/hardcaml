open! Import

(* Demonstrate a special case when instantiating a RTL design which has a single output of
   1 bit - this requires a slightly different syntax in the port association map. *)

let instantiation_circuit =
  lazy
    (let a = Signal.input "a" 1 in
     let inst =
       Instantiation.create () ~name:"example" ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
     in
     Circuit.create_exn ~name:"example" [ Signal.output "b" (inst#o "b") ])
;;

let%expect_test "Intstantiation in Verilog with single bit output" =
  Rtl.print Verilog (force instantiation_circuit);
  [%expect
    {|
    module example (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */
        wire _4;

        /* logic */
        example
            the_example
            ( .a(a), .b(_4) );

        /* aliases */

        /* output assignments */
        assign b = _4;

    endmodule |}]
;;

let%expect_test "Intstantiation in VHDL with single bit output" =
  Rtl.print Vhdl (force instantiation_circuit);
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity example is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of example is

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

        -- signal declarations
        signal hc_4 : std_logic;

    begin

        -- logic
        the_example: entity work.example (rtl)
            port map ( a => a, b => hc_4 );

        -- aliases

        -- output assignments
        b <= hc_4;

    end architecture; |}]
;;

(* Another instantiation test, this time with multiple inputs and outputs *)

let instantiation_circuit =
  lazy
    (let a = Signal.input "a" 1 in
     let b = Signal.input "b" 3 in
     let inst =
       Instantiation.create
         ()
         ~name:"example"
         ~inputs:[ "a", a; "b", b ]
         ~outputs:[ "c", 1; "d", 4 ]
     in
     Circuit.create_exn
       ~name:"example"
       [ Signal.output "c" (inst#o "c"); Signal.output "d" (inst#o "d") ])
;;

let%expect_test "Intstantiation in Verilog with multiple inputs and outputs" =
  Rtl.print Verilog (force instantiation_circuit);
  [%expect
    {|
    module example (
        b,
        a,
        c,
        d
    );

        input [2:0] b;
        input a;
        output c;
        output [3:0] d;

        /* signal declarations */
        wire [3:0] _7;
        wire [4:0] _6;
        wire _8;

        /* logic */
        assign _7 = _6[4:1];
        example
            the_example
            ( .a(a), .b(b), .d(_6[4:1]), .c(_6[0:0]) );
        assign _8 = _6[0:0];

        /* aliases */

        /* output assignments */
        assign c = _8;
        assign d = _7;

    endmodule |}]
;;
