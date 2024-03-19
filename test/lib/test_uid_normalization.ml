open! Import
open Signal

let%expect_test "combinational loop" =
  let a = input "a" 2 in
  let w = wire 2 in
  let b = a +: w in
  w <== b;
  let signal_graph = Signal_graph.create [ b ] in
  print_s [%sexp (Signal_graph.normalize_uids signal_graph : Signal_graph.t)];
  [%expect {| ((outputs ((add (width 2) (arguments (a wire))))) (upto ())) |}]
;;

let design () =
  let a = input "a" 2 in
  let b = input "b" 2 in
  let x =
    Instantiation.create
      ()
      ~name:"blah"
      ~inputs:[ "a", a; "b", b ]
      ~outputs:[ "c", 3; "d", 3; "e", 3 ]
  in
  let m =
    memory
      4
      ~write_port:
        { write_clock = clock; write_enable = bit a 1; write_address = b; write_data = a }
      ~read_address:a
  in
  let w = wireof a in
  let reg_spec = Reg_spec.create () ~clock ~clear in
  [ output "c" (reg reg_spec ~enable:vdd (a +: b))
  ; output "d" (mux2 vdd a b)
  ; output "e" (Map.find_exn x "c" |: Map.find_exn x "d" |: Map.find_exn x "e")
  ; output "f" m
  ; output "g" m
  ; output "h" w
  ]
;;

let test ~normalize_uids =
  let circuit =
    Circuit.create_exn
      ~config:{ Circuit.Config.default with normalize_uids }
      ~name:"foo"
      (design ())
  in
  Rtl.print Verilog circuit
;;

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
  [%expect
    {|
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

        wire [1:0] _1;
        wire _12;
        reg [1:0] _13[0:3];
        wire [1:0] _14;
        wire [2:0] _20;
        wire [2:0] _18;
        wire [8:0] _16;
        wire [2:0] _17;
        wire [2:0] _19;
        wire [2:0] _21;
        wire vdd;
        wire [1:0] _24;
        wire [1:0] _22;
        reg [1:0] _26;
        assign _1 = a;
        assign _12 = a[1:1];
        always @(posedge clock) begin
            if (_12)
                _13[b] <= a;
        end
        assign _14 = _13[a];
        assign _20 = _16[8:6];
        assign _18 = _16[5:3];
        blah
            the_blah
            ( .a(a),
              .b(b),
              .e(_16[8:6]),
              .d(_16[5:3]),
              .c(_16[2:0]) );
        assign _17 = _16[2:0];
        assign _19 = _17 | _18;
        assign _21 = _19 | _20;
        assign vdd = 1'b1;
        assign _24 = 2'b00;
        assign _22 = a + b;
        always @(posedge clock) begin
            if (clear)
                _26 <= _24;
            else
                _26 <= _22;
        end
        assign c = _26;
        assign d = a;
        assign e = _21;
        assign f = _14;
        assign g = _14;
        assign h = _1;

    endmodule
    |}]
;;
