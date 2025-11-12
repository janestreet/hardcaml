open! Import

(* Demonstrate a special case when instantiating a RTL design which has a single output of
   1 bit - this requires a slightly different syntax in the port association map. *)

let instantiation_circuit =
  lazy
    (let a = Signal.input "a" 1 in
     let inst =
       Instantiation.create () ~name:"example" ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
     in
     Circuit.create_exn
       ~name:"example"
       [ Signal.output "b" (Instantiation.output inst "b") ])
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

        wire _4;
        wire _2;
        example
            the_example
            ( .a(a),
              .b(_4) );
        assign _2 = _4;
        assign b = _2;

    endmodule
    |}]
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

        signal \_4\ : std_logic;
        signal \_2\ : std_logic;

    begin

        the_example: entity work.example (rtl)
            port map ( a => a,
                       b => \_4\ );
        \_2\ <= \_4\;
        b <= \_2\;

    end architecture;
    |}]
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
       [ Signal.output "c" (Instantiation.output inst "c")
       ; Signal.output "d" (Instantiation.output inst "d")
       ])
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

        wire [3:0] _6;
        wire [4:0] _5;
        wire _7;
        assign _6 = _5[4:1];
        example
            the_example
            ( .a(a),
              .b(b),
              .c(_5[0:0]),
              .d(_5[4:1]) );
        assign _7 = _5[0:0];
        assign c = _7;
        assign d = _6;

    endmodule
    |}]
;;
