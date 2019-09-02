open! Import
open Signal

let f_coefs = Test_fir_filter.f (List.init 4 ~f:(fun _ -> random ~width:16))

let%expect_test "top level blackbox" =
  let module Circuit = Circuit.With_interface (Test_fir_filter.I) (Test_fir_filter.O) in
  let circuit = Circuit.create_exn ~name:"fir_filter_blackbox" f_coefs in
  Rtl.print ~blackbox:Top Verilog circuit;
  [%expect
    {|
    module fir_filter_blackbox (
        enable,
        clr,
        clk,
        d,
        q
    );

        input enable;
        input clr;
        input clk;
        input [15:0] d;
        output [31:0] q;

    endmodule |}]
;;

let f_inst scope =
  let module Hierarchy = Hierarchy.In_scope (Test_fir_filter.I) (Test_fir_filter.O) in
  Hierarchy.hierarchical ~scope ~name:"fir_filter" (fun _scope -> f_coefs)
;;

let%expect_test "Instantiation blackbox" =
  let module Circuit = Circuit.With_interface (Test_fir_filter.I) (Test_fir_filter.O) in
  let scope = Scope.create ~flatten_design:false () in
  let circuit = Circuit.create_exn ~name:"fir_filter_top" (f_inst scope) in
  (* Print the whole thing, include the fir filter sub-circuit. *)
  Rtl.print ~database:(Scope.circuit_database scope) ~blackbox:None Verilog circuit;
  [%expect
    {|
    module fir_filter (
        enable,
        clr,
        clk,
        d,
        q
    );

        input enable;
        input clr;
        input clk;
        input [15:0] d;
        output [31:0] q;

        /* signal declarations */
        wire [15:0] _29 = 16'b0110000111000000;
        wire [31:0] _30;
        wire [15:0] _27 = 16'b0011100010011101;
        wire [31:0] _28;
        wire [31:0] _31;
        wire [15:0] _24 = 16'b1100110001010110;
        wire [31:0] _25;
        wire [15:0] _22 = 16'b0110100000011001;
        wire [15:0] _20 = 16'b0000000000000000;
        wire [15:0] _19 = 16'b0000000000000000;
        wire [15:0] _17 = 16'b0000000000000000;
        wire [15:0] _16 = 16'b0000000000000000;
        wire [15:0] _14 = 16'b0000000000000000;
        wire [15:0] _13 = 16'b0000000000000000;
        wire _2;
        wire [15:0] _11 = 16'b0000000000000000;
        wire _4;
        wire [15:0] _10 = 16'b0000000000000000;
        wire _6;
        wire [15:0] _8;
        reg [15:0] _12;
        reg [15:0] _15;
        reg [15:0] _18;
        reg [15:0] _21;
        wire [31:0] _23;
        wire [31:0] _26;
        wire [31:0] _32;

        /* logic */
        assign _30 = $signed(_12) * $signed(_29);
        assign _28 = $signed(_15) * $signed(_27);
        assign _31 = _28 + _30;
        assign _25 = $signed(_18) * $signed(_24);
        assign _2 = enable;
        assign _4 = clr;
        assign _6 = clk;
        assign _8 = d;
        always @(posedge _6) begin
            if (_4)
                _12 <= _11;
            else
                if (_2)
                    _12 <= _8;
        end
        always @(posedge _6) begin
            if (_4)
                _15 <= _14;
            else
                if (_2)
                    _15 <= _12;
        end
        always @(posedge _6) begin
            if (_4)
                _18 <= _17;
            else
                if (_2)
                    _18 <= _15;
        end
        always @(posedge _6) begin
            if (_4)
                _21 <= _20;
            else
                if (_2)
                    _21 <= _18;
        end
        assign _23 = $signed(_21) * $signed(_22);
        assign _26 = _23 + _25;
        assign _32 = _26 + _31;

        /* aliases */

        /* output assignments */
        assign q = _32;

    endmodule
    module fir_filter_top (
        d,
        enable,
        clr,
        clk,
        q
    );

        input [15:0] d;
        input enable;
        input clr;
        input clk;
        output [31:0] q;

        /* signal declarations */
        wire [15:0] _2;
        wire _4;
        wire _6;
        wire _8;
        wire [31:0] _11;

        /* logic */
        assign _2 = d;
        assign _4 = enable;
        assign _6 = clr;
        assign _8 = clk;
        fir_filter
            the_fir_filter
            ( .clk(_8), .clr(_6), .enable(_4), .d(_2), .q(_11[31:0]) );

        /* aliases */

        /* output assignments */
        assign q = _11;

    endmodule |}];
  (* Now just print the top level module, plus black boxes for the instantiations *)
  Rtl.print
    ~database:(Scope.circuit_database scope)
    ~blackbox:Instantiations
    Verilog
    circuit;
  [%expect
    {|
    module fir_filter (
        enable,
        clr,
        clk,
        d,
        q
    );

        input enable;
        input clr;
        input clk;
        input [15:0] d;
        output [31:0] q;

    endmodule
    module fir_filter_top (
        d,
        enable,
        clr,
        clk,
        q
    );

        input [15:0] d;
        input enable;
        input clr;
        input clk;
        output [31:0] q;

        /* signal declarations */
        wire [15:0] _2;
        wire _4;
        wire _6;
        wire _8;
        wire [31:0] _11;

        /* logic */
        assign _2 = d;
        assign _4 = enable;
        assign _6 = clr;
        assign _8 = clk;
        fir_filter
            the_fir_filter
            ( .clk(_8), .clr(_6), .enable(_4), .d(_2), .q(_11[31:0]) );

        /* aliases */

        /* output assignments */
        assign q = _11;

    endmodule |}]
;;
