open! Import
open Signal

let rtl_write_null lang outputs = Rtl.print lang (Circuit.create_exn ~name:"test" outputs)

let output =
  let a = add_attribute (wire 4 -- "a") (Rtl_attribute.create "baz" ~value:(Bool true)) in
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
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { c : 'a [@bits 4] } [@@deriving hardcaml]
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
        (* baz=1 *)
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
  require_does_not_raise [%here] (fun () -> rtl_write_null Vhdl [ output ]);
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity test is
        port (
            b : in std_logic_vector (3 downto 0);
            a : in std_logic_vector (3 downto 0);
            result : out std_logic_vector (3 downto 0)
        );
    end entity;

    architecture rtl of test is

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
        constant hc_5 : std_logic_vector (3 downto 0) := "0011";
        signal hc_4 : std_logic_vector (3 downto 0);
        signal tmp : std_logic_vector (3 downto 0);

    begin

        -- logic
        hc_4 <= hc_slv(hc_uns(a) + hc_uns(b));
        tmp <= hc_slv(hc_uns(hc_4) + hc_uns(hc_5));

        -- aliases

        -- output assignments
        result <= tmp;

    end architecture; |}]
;;

let%expect_test "Test Rtl attributes on the pipeline construct" =
  let module Test_pipeline = struct
    module I = struct
      type 'a t =
        { clk : 'a
        ; a : 'a [@bits 4]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { b : 'a [@bits 4] } [@@deriving hardcaml]
    end

    let create (i : _ I.t) =
      let spec = Reg_spec.create ~clock:i.clk () in
      { O.b =
          pipeline
            ~attributes:[ Rtl_attribute.Vivado.Srl_style.register ]
            spec
            ~enable:vdd
            ~n:3
            i.a
      }
    ;;
  end
  in
  let open Test_pipeline in
  let module Circuit = Circuit.With_interface (I) (O) in
  Rtl.print Verilog (Circuit.create_exn ~name:"module_foo" create);
  [%expect
    {|
    module module_foo (
        clk,
        a,
        b
    );

        input clk;
        input [3:0] a;
        (* SRL_STYLE="register" *)
        output [3:0] b;

        /* signal declarations */
        wire [3:0] _14 = 4'b0000;
        wire [3:0] _13 = 4'b0000;
        wire [3:0] _11 = 4'b0000;
        wire [3:0] _10 = 4'b0000;
        wire vdd = 1'b1;
        wire [3:0] _7 = 4'b0000;
        wire [3:0] _6 = 4'b0000;
        wire _2;
        wire [3:0] _4;
        (* SRL_STYLE="register" *)
        reg [3:0] _9;
        (* SRL_STYLE="register" *)
        reg [3:0] _12;
        reg [3:0] _15;

        /* logic */
        assign _2 = clk;
        assign _4 = a;
        always @(posedge _2) begin
            _9 <= _4;
        end
        always @(posedge _2) begin
            _12 <= _9;
        end
        always @(posedge _2) begin
            _15 <= _12;
        end

        /* aliases */

        /* output assignments */
        assign b = _15;

    endmodule |}]
;;
