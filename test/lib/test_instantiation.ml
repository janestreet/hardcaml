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

        wire signal_inst;
        wire signal_wire;
        example
            the_example
            ( .a(a),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        assign b = signal_wire;

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

        signal signal_inst : std_logic;
        signal signal_wire : std_logic;

    begin

        the_example: entity work.example (rtl)
            port map ( a => a,
                       b => signal_inst );
        signal_wire <= signal_inst;
        b <= signal_wire;

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

        wire [3:0] signal_select;
        wire [4:0] signal_inst;
        wire signal_select_1;
        assign signal_select = signal_inst[4:1];
        example
            the_example
            ( .a(a),
              .b(b),
              .c(signal_inst[0:0]),
              .d(signal_inst[4:1]) );
        assign signal_select_1 = signal_inst[0:0];
        assign c = signal_select_1;
        assign d = signal_select;

    endmodule
    |}]
;;
