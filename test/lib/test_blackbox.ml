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
        clk,
        clr,
        enable,
        d,
        q
    );

        input clk;
        input clr;
        input enable;
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
        clk,
        clr,
        enable,
        d,
        q
    );

        input clk;
        input clr;
        input enable;
        input [15:0] d;
        output [31:0] q;

        wire [15:0] signal_const;
        wire [31:0] signal_muls;
        wire [15:0] signal_const_1;
        wire [31:0] signal_muls_1;
        wire [31:0] signal_add;
        wire [15:0] signal_const_2;
        wire [31:0] signal_muls_2;
        wire [15:0] signal_const_3;
        wire [15:0] signal_const_4;
        wire signal_wire;
        wire signal_wire_1;
        wire signal_wire_2;
        wire [15:0] signal_wire_3;
        reg [15:0] signal_reg;
        reg [15:0] signal_reg_1;
        reg [15:0] signal_reg_2;
        reg [15:0] signal_reg_3;
        wire [31:0] signal_muls_3;
        wire [31:0] signal_add_1;
        wire [31:0] signal_add_2;
        assign signal_const = 16'b1001000110011010;
        assign signal_muls = $signed(signal_reg) * $signed(signal_const);
        assign signal_const_1 = 16'b0000010001101110;
        assign signal_muls_1 = $signed(signal_reg_1) * $signed(signal_const_1);
        assign signal_add = signal_muls_1 + signal_muls;
        assign signal_const_2 = 16'b1011100100000110;
        assign signal_muls_2 = $signed(signal_reg_2) * $signed(signal_const_2);
        assign signal_const_3 = 16'b0101011010100001;
        assign signal_const_4 = 16'b0000000000000000;
        assign signal_wire = enable;
        assign signal_wire_1 = clr;
        assign signal_wire_2 = clk;
        assign signal_wire_3 = d;
        always @(posedge signal_wire_2) begin
            if (signal_wire_1)
                signal_reg <= signal_const_4;
            else
                if (signal_wire)
                    signal_reg <= signal_wire_3;
        end
        always @(posedge signal_wire_2) begin
            if (signal_wire_1)
                signal_reg_1 <= signal_const_4;
            else
                if (signal_wire)
                    signal_reg_1 <= signal_reg;
        end
        always @(posedge signal_wire_2) begin
            if (signal_wire_1)
                signal_reg_2 <= signal_const_4;
            else
                if (signal_wire)
                    signal_reg_2 <= signal_reg_1;
        end
        always @(posedge signal_wire_2) begin
            if (signal_wire_1)
                signal_reg_3 <= signal_const_4;
            else
                if (signal_wire)
                    signal_reg_3 <= signal_reg_2;
        end
        assign signal_muls_3 = $signed(signal_reg_3) * $signed(signal_const_3);
        assign signal_add_1 = signal_muls_3 + signal_muls_2;
        assign signal_add_2 = signal_add_1 + signal_add;
        assign q = signal_add_2;

    endmodule
    module fir_filter_top (
        clk,
        clr,
        enable,
        d,
        q
    );

        input clk;
        input clr;
        input enable;
        input [15:0] d;
        output [31:0] q;

        wire [15:0] signal_wire;
        wire signal_wire_1;
        wire signal_wire_2;
        wire signal_wire_3;
        wire [31:0] signal_inst;
        wire [31:0] signal_wire_4;
        assign signal_wire = d;
        assign signal_wire_1 = enable;
        assign signal_wire_2 = clr;
        assign signal_wire_3 = clk;
        fir_filter
            fir_filter
            ( .clk(signal_wire_3),
              .clr(signal_wire_2),
              .enable(signal_wire_1),
              .d(signal_wire),
              .q(signal_inst[31:0]) );
        assign signal_wire_4 = signal_inst;
        assign q = signal_wire_4;

    endmodule
    |}];
  (* Now just print the top level module, plus black boxes for the instantiations *)
  print ~scope circuit Rtl.top_levels_and_blackboxes;
  [%expect
    {|
    module fir_filter (
        clk,
        clr,
        enable,
        d,
        q
    );

        input clk;
        input clr;
        input enable;
        input [15:0] d;
        output [31:0] q;


    endmodule
    module fir_filter_top (
        clk,
        clr,
        enable,
        d,
        q
    );

        input clk;
        input clr;
        input enable;
        input [15:0] d;
        output [31:0] q;

        wire [15:0] signal_wire;
        wire signal_wire_1;
        wire signal_wire_2;
        wire signal_wire_3;
        wire [31:0] signal_inst;
        wire [31:0] signal_wire_4;
        assign signal_wire = d;
        assign signal_wire_1 = enable;
        assign signal_wire_2 = clr;
        assign signal_wire_3 = clk;
        fir_filter
            fir_filter
            ( .clk(signal_wire_3),
              .clr(signal_wire_2),
              .enable(signal_wire_1),
              .d(signal_wire),
              .q(signal_inst[31:0]) );
        assign signal_wire_4 = signal_inst;
        assign q = signal_wire_4;

    endmodule
    |}]
;;
