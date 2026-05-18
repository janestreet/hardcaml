open! Import
open Signal

let%expect_test "combinational loop" =
  let a = input "a" 2 in
  let w = wire 2 in
  let b = a +: w in
  w <-- b;
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
        { write_clock = clock; write_enable = a.:(1); write_address = b; write_data = a }
      ~read_address:a
  in
  let w = wireof a in
  let reg_spec = Reg_spec.create () ~clock ~clear in
  [ output "c" (reg reg_spec ~enable:vdd (a +: b))
  ; output "d" (mux2 vdd a b)
  ; output
      "e"
      (Instantiation.output x "c"
       |: Instantiation.output x "d"
       |: Instantiation.output x "e")
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

        wire [1:0] signal_wire;
        wire signal_select;
        reg [1:0] signal_multiport_mem[0:3];
        wire [1:0] signal_mem_read_port;
        wire [2:0] signal_select_1;
        wire [2:0] signal_select_2;
        wire [8:0] signal_inst;
        wire [2:0] signal_select_3;
        wire [2:0] signal_or;
        wire [2:0] signal_or_1;
        wire [1:0] signal_const;
        wire [1:0] signal_add;
        reg [1:0] signal_reg;
        assign signal_wire = a;
        assign signal_select = a[1:1];
        always @(posedge clock) begin
            if (signal_select)
                signal_multiport_mem[b] <= a;
        end
        assign signal_mem_read_port = signal_multiport_mem[a];
        assign signal_select_1 = signal_inst[8:6];
        assign signal_select_2 = signal_inst[5:3];
        blah
            the_blah
            ( .a(a),
              .b(b),
              .c(signal_inst[2:0]),
              .d(signal_inst[5:3]),
              .e(signal_inst[8:6]) );
        assign signal_select_3 = signal_inst[2:0];
        assign signal_or = signal_select_3 | signal_select_2;
        assign signal_or_1 = signal_or | signal_select_1;
        assign signal_const = 2'b00;
        assign signal_add = a + b;
        always @(posedge clock) begin
            if (clear)
                signal_reg <= signal_const;
            else
                signal_reg <= signal_add;
        end
        assign c = signal_reg;
        assign d = a;
        assign e = signal_or_1;
        assign f = signal_mem_read_port;
        assign g = signal_mem_read_port;
        assign h = signal_wire;

    endmodule
    |}]
;;
