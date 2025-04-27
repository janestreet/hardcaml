open! Import
open Signal

let f_coefs = Test_fir_filter.f (List.init 4 ~f:(fun _ -> random ~width:16))

let print ?scope circuit f =
  Rtl.create ?database:(Option.map scope ~f:Scope.circuit_database) Verilog [ circuit ]
  |> f
  |> Rope.to_string
  |> print_string
;;

let%expect_test "top level blackbox" =
  let module Circuit = Circuit.With_interface (Test_fir_filter.I) (Test_fir_filter.O) in
  let circuit = Circuit.create_exn ~name:"fir_filter_blackbox" f_coefs in
  print circuit Rtl.top_levels_as_blackboxes;
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


    endmodule
    |}]
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
  print ~scope circuit Rtl.full_hierarchy;
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

        wire [15:0] _25;
        wire [31:0] _26;
        wire [15:0] _23;
        wire [31:0] _24;
        wire [31:0] _27;
        wire [15:0] _20;
        wire [31:0] _21;
        wire [15:0] _18;
        wire [15:0] _16;
        wire _2;
        wire _4;
        wire _6;
        wire [15:0] _8;
        reg [15:0] _11;
        reg [15:0] _13;
        reg [15:0] _15;
        reg [15:0] _17;
        wire [31:0] _19;
        wire [31:0] _22;
        wire [31:0] _28;
        assign _25 = 16'b1001000110011010;
        assign _26 = $signed(_11) * $signed(_25);
        assign _23 = 16'b0000010001101110;
        assign _24 = $signed(_13) * $signed(_23);
        assign _27 = _24 + _26;
        assign _20 = 16'b1011100100000110;
        assign _21 = $signed(_15) * $signed(_20);
        assign _18 = 16'b0101011010100001;
        assign _16 = 16'b0000000000000000;
        assign _2 = enable;
        assign _4 = clr;
        assign _6 = clk;
        assign _8 = d;
        always @(posedge _6) begin
            if (_4)
                _11 <= _16;
            else
                if (_2)
                    _11 <= _8;
        end
        always @(posedge _6) begin
            if (_4)
                _13 <= _16;
            else
                if (_2)
                    _13 <= _11;
        end
        always @(posedge _6) begin
            if (_4)
                _15 <= _16;
            else
                if (_2)
                    _15 <= _13;
        end
        always @(posedge _6) begin
            if (_4)
                _17 <= _16;
            else
                if (_2)
                    _17 <= _15;
        end
        assign _19 = $signed(_17) * $signed(_18);
        assign _22 = _19 + _21;
        assign _28 = _22 + _27;
        assign q = _28;

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

        wire [15:0] _2;
        wire _4;
        wire _6;
        wire _8;
        wire [31:0] _11;
        wire [31:0] _9;
        assign _2 = d;
        assign _4 = enable;
        assign _6 = clr;
        assign _8 = clk;
        fir_filter
            fir_filter
            ( .clk(_8),
              .clr(_6),
              .enable(_4),
              .d(_2),
              .q(_11[31:0]) );
        assign _9 = _11;
        assign q = _9;

    endmodule
    |}];
  (* Now just print the top level module, plus black boxes for the instantiations *)
  print ~scope circuit Rtl.top_levels_and_blackboxes;
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

        wire [15:0] _2;
        wire _4;
        wire _6;
        wire _8;
        wire [31:0] _11;
        wire [31:0] _9;
        assign _2 = d;
        assign _4 = enable;
        assign _6 = clr;
        assign _8 = clk;
        fir_filter
            fir_filter
            ( .clk(_8),
              .clr(_6),
              .enable(_4),
              .d(_2),
              .q(_11[31:0]) );
        assign _9 = _11;
        assign q = _9;

    endmodule
    |}]
;;
