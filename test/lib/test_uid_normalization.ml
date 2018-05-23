open! Import
open Signal

let%expect_test "combinational loop" =
  let a = input "a" 2 in
  let w = wire 2 in
  let b = a +: w in
  w <== b;
  let signal_graph = Signal_graph.create [ b ] in
  print_s [%sexp (Signal_graph.normalize_uids signal_graph : Signal_graph.t)];
  [%expect {|
    ((add (width 2) (arguments (a wire)))) |}];
;;

let design () =
  let a = input "a" 2 in
  let b = input "b" 2 in
  let x = Instantiation.create ()
            ~name:"blah"
            ~inputs:[ "a", a; "b", b ]
            ~outputs:[ "c", 3; "d", 3; "e", 3 ] in
  let ram_spec = Ram_spec.create () ~clk:clock in
  let m = memory (Ram_spec.override ram_spec ~c:clear) ~we:(bit a 1) ~wa:b ~d:a ~ra:a 4 in
  let w = wireof a in
  let reg_spec = Reg_spec.create () ~clk:clock ~clr:clear in
  [ output "c" (reg reg_spec ~e:vdd (a +: b))
  ; output "d" (mux2 vdd a b)
  ; output "e" (x#o "c" |: x#o "d" |: x#o "e")
  ; output "f" m
  ; output "g" m
  ; output "h" w ]

let test ~normalize_uids =
  let circuit = Circuit.create_exn ~normalize_uids ~name:"foo" (design ()) in
  Rtl.print Verilog circuit

(* The following test is now commented out as it is brittle to changes in the test
   environment.

   {v
let%expect_test "verilog without normalization" =
  test ~normalize_uids:false;
  [%expect {|
    module foo (
        clear,
        clock,
        b,
        a,
        c,
        d,
        e,
        f,
        g,
        h
    );

        input clear;
        input clock;
        input [1:0] b;
        input [1:0] a;
        output [1:0] c;
        output [1:0] d;
        output [2:0] e;
        output [1:0] f;
        output [1:0] g;
        output [1:0] h;

        /* signal declarations */
        wire [1:0] _56;
        wire [1:0] _52 = 2'b00;
        wire [1:0] _53 = 2'b00;
        wire _51;
        wire [1:0] _55;
        reg [1:0] _55_mem[0:3];
        wire [2:0] _60;
        wire [2:0] _61;
        wire [8:0] _50;
        wire [2:0] _62;
        wire [2:0] _63;
        wire [2:0] _64;
        wire [1:0] _66;
        wire [1:0] _69 = 2'b00;
        wire vdd = 1'b1;
        wire [1:0] _70 = 2'b00;
        wire [1:0] _68;
        reg [1:0] _71;

        /* logic */
        assign _56 = a;
        assign _51 = a[1:1];
        always @(posedge clock) begin
            if (clear)
                begin: _55_blk
                    integer _55_idx;
                    for (_55_idx=0; _55_idx<4; _55_idx=_55_idx+1)
                        _55_mem[_55_idx] <= _52;
                end
            else
                if (_51)
                    _55_mem[b] <= a;
        end
        assign _55 = _55_mem[a];
        assign _60 = _50[8:6];
        assign _61 = _50[5:3];
        blah
            the_blah
            ( .a(a), .b(b), .e(_50[8:6]), .d(_50[5:3]), .c(_50[2:0]) );
        assign _62 = _50[2:0];
        assign _63 = _62 | _61;
        assign _64 = _63 | _60;
        assign _66 = vdd ? a : b;
        assign _68 = a + b;
        always @(posedge clock) begin
            if (clear)
                _71 <= _69;
            else
                _71 <= _68;
        end

        /* aliases */

        /* output assignments */
        assign c = _71;
        assign d = _66;
        assign e = _64;
        assign f = _55;
        assign g = _55;
        assign h = _56;

    endmodule |}]
v} *)

let%expect_test "verilog with normalization" =
  test ~normalize_uids:true;
  [%expect {|
    module foo (
        clear,
        clock,
        b,
        a,
        c,
        d,
        e,
        f,
        g,
        h
    );

        input clear;
        input clock;
        input [1:0] b;
        input [1:0] a;
        output [1:0] c;
        output [1:0] d;
        output [2:0] e;
        output [1:0] f;
        output [1:0] g;
        output [1:0] h;

        /* signal declarations */
        wire [1:0] _1;
        wire [1:0] _15 = 2'b00;
        wire [1:0] _13 = 2'b00;
        wire _12;
        wire [1:0] _17;
        reg [1:0] _17_mem[0:3];
        wire [2:0] _23;
        wire [2:0] _21;
        wire [8:0] _19;
        wire [2:0] _20;
        wire [2:0] _22;
        wire [2:0] _24;
        wire [1:0] _25;
        wire [1:0] _28 = 2'b00;
        wire vdd = 1'b1;
        wire [1:0] _27 = 2'b00;
        wire [1:0] _26;
        reg [1:0] _29;

        /* logic */
        assign _1 = a;
        assign _12 = a[1:1];
        always @(posedge clock) begin
            if (clear)
                begin: _17_blk
                    integer _17_idx;
                    for (_17_idx=0; _17_idx<4; _17_idx=_17_idx+1)
                        _17_mem[_17_idx] <= _15;
                end
            else
                if (_12)
                    _17_mem[b] <= a;
        end
        assign _17 = _17_mem[a];
        assign _23 = _19[8:6];
        assign _21 = _19[5:3];
        blah
            the_blah
            ( .a(a), .b(b), .e(_19[8:6]), .d(_19[5:3]), .c(_19[2:0]) );
        assign _20 = _19[2:0];
        assign _22 = _20 | _21;
        assign _24 = _22 | _23;
        assign _25 = vdd ? a : b;
        assign _26 = a + b;
        always @(posedge clock) begin
            if (clear)
                _29 <= _28;
            else
                _29 <= _26;
        end

        /* aliases */

        /* output assignments */
        assign c = _29;
        assign d = _25;
        assign e = _24;
        assign f = _17;
        assign g = _17;
        assign h = _1;

    endmodule |}]
