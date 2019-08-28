open! Import
open Signal

let rtl_write_null lang outputs =
  Rtl.print lang (Circuit.create_exn ~name:"test" outputs)
;;

let output =
  let a =
    add_attribute (wire 4 -- "a") (Rtl_attribute.create "baz" ~value:(Bool true))
  in
  let b =
    add_attribute
      (add_attribute (wire 4 -- "b") (Rtl_attribute.create "bar" ~value:(Int 10)))
      (Rtl_attribute.create "bla")
  in
  let output = wire 4 -- "result" in
  output
  <== add_attribute (a +: b +:. 3) (Rtl_attribute.create "hello" ~value:(String "world"))
      -- "tmp";
  output
;;

module Test_component = struct
  module I = struct
    type 'a t =
      { clk : 'a
      ; clear : 'a
      ; a : 'a [@bits 4]
      ; b : 'a [@bits 4]
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t = { c : 'a [@bits 4] } [@@deriving sexp_of, hardcaml]
  end

  let create (i : _ I.t) =
    let reg_spec = Reg_spec.create ~clock:i.clk () in
    let a = reg reg_spec ~enable:vdd i.a -- "hello" in
    let a = add_attribute a (Rtl_attribute.Vivado.dont_touch true) in
    let b = add_attribute i.b (Rtl_attribute.Vivado.mark_debug true) in
    let c = add_attribute (a +: b) (Rtl_attribute.Vivado.mark_debug true) in
    { O.c }
  ;;
end

let%expect_test "Signal attributes on top of signals in Verilog for circuitsconstructed \
                 using Circuit.With_interface"
  =
  let open Test_component in
  let module Circuit = Circuit.With_interface (I) (O) in
  Rtl.print Verilog (Circuit.create_exn ~name:"module_foo" create);
  [%expect
    {|
    module module_foo (
        b,
        clk,
        a,
        clear,
        c
    );

        (* mark_debug="TRUE" *)
        input [3:0] b;
        input clk;
        input [3:0] a;
        input clear;
        (* mark_debug="TRUE" *)
        output [3:0] c;

        /* signal declarations */
        wire [3:0] _2;
        wire vdd = 1'b1;
        wire [3:0] _9 = 4'b0000;
        wire [3:0] _8 = 4'b0000;
        wire _4;
        wire [3:0] _6;
        (* dont_touch="TRUE" *)
        reg [3:0] hello;
        wire [3:0] _12;

        /* logic */
        assign _2 = b;
        assign _4 = clk;
        assign _6 = a;
        always @(posedge _4) begin
            hello <= _6;
        end
        assign _12 = hello + _2;

        /* aliases */

        /* output assignments */
        assign c = _12;

    endmodule |}]
;;

let%expect_test "Signal attributes on top of signals in Verilog" =
  rtl_write_null Verilog [ output ];
  [%expect
    {|
    module test (
        b,
        a,
        result
    );

        (* bla,bar=10 *)
        input [3:0] b;
        (* baz=true *)
        input [3:0] a;
        output [3:0] result;

        /* signal declarations */
        wire [3:0] _5 = 4'b0011;
        wire [3:0] _4;
        (* hello="world" *)
        wire [3:0] tmp;

        /* logic */
        assign _4 = a + b;
        assign tmp = _4 + _5;

        /* aliases */

        /* output assignments */
        assign result = tmp;

    endmodule |}]
;;

let%expect_test "Signal attributes on top of signals in VHDL" =
  require_does_raise [%here] (fun () -> rtl_write_null Vhdl [ output ]);
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name test)
      (hierarchy_path (test))
      (output ((language Vhdl) (mode (To_channel <stdout>))))
      (exn "Signal attributes are not supported in VHDL yet")) |}]
;;
