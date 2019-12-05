open! Import

let%expect_test "no outputs" =
  require_does_raise [%here] (fun () ->
    Combinational_op.create
      ()
      ~name:"no_outputs"
      ~input_widths:[]
      ~output_widths:[]
      ~create_fn:(fun _ -> []));
  [%expect {|
    "[Combinational_op]s require at least one output" |}]
;;

let%expect_test "no inputs" =
  require_does_not_raise [%here] (fun () ->
    ignore
      (Combinational_op.create
         ()
         ~name:"no_inputs"
         ~input_widths:[]
         ~output_widths:[ 1 ]
         ~create_fn:(fun _ -> [])
       : Combinational_op.t));
  [%expect {| |}]
;;

let%expect_test "bad output width" =
  require_does_raise [%here] (fun () ->
    Combinational_op.create
      ()
      ~name:"bad_width"
      ~input_widths:[ 1 ]
      ~output_widths:[ 0 ]
      ~create_fn:(fun _ -> []));
  [%expect {| ("[Combinational_op] output width <=0" (width 0)) |}]
;;

let%expect_test "bad input width" =
  require_does_raise [%here] (fun () ->
    Combinational_op.create
      ()
      ~name:"bad_width"
      ~input_widths:[ 0 ]
      ~output_widths:[ 1 ]
      ~create_fn:(fun _ -> []));
  [%expect {| ("[Combinational_op] input width <=0" (width 0)) |}]
;;

let%expect_test "names must be unique in database" =
  let op =
    Combinational_op.create
      ()
      ~name:"foo"
      ~input_widths:[ 1 ]
      ~output_widths:[ 1 ]
      ~create_fn:(fun a -> a)
  in
  let database = Combinational_ops_database.create () in
  Combinational_ops_database.insert database op;
  require_does_raise [%here] (fun () -> Combinational_ops_database.insert database op);
  [%expect
    {|
    ("A [Combinational_op] of the same name already exists in the database"
     (name foo)) |}]
;;

(* test add_sub; a 2 input, 2 output function *)

let num_bits = 8

let create_op () =
  Combinational_op.create
    ()
    ~name:"add_sub"
    ~input_widths:[ num_bits; num_bits ]
    ~output_widths:[ num_bits; num_bits ]
    ~create_fn:(function
      | [ a; b ] -> Bits.[ a +: b; a -: b ]
      | _ -> raise_s [%message "invalid arguments"])
;;

let create_op_mutable () =
  Combinational_op.create
    ()
    ~name:"add_sub_mutable"
    ~input_widths:[ num_bits; num_bits ]
    ~output_widths:[ num_bits; num_bits ]
    ~create_fn_mutable:(fun i o ->
      match i, o with
      | [ a; b ], [ c; d ] ->
        Bits.Mutable.( +: ) c a b;
        Bits.Mutable.( -: ) d a b
      | _ -> raise_s [%message "invalid arguments"])
;;

let%expect_test "sexp_of" =
  let database = Combinational_ops_database.create () in
  Combinational_ops_database.insert database (create_op ());
  Combinational_ops_database.insert database (create_op_mutable ());
  print_s [%sexp (database : Combinational_ops_database.t)];
  [%expect
    {|
    ((
      by_name (
        (add_sub (
          (name add_sub)
          (input_widths      (8       8))
          (output_widths     (8       8))
          (create_fn         (Native  <opaque>))
          (create_fn_mutable (Derived <opaque>))))
        (add_sub_mutable (
          (name add_sub_mutable)
          (input_widths      (8       8))
          (output_widths     (8       8))
          (create_fn         (Derived <opaque>))
          (create_fn_mutable (Native  <opaque>))))))) |}]
;;

let create_circuit create_op () =
  let database = Combinational_ops_database.create () in
  let op = create_op () in
  Combinational_ops_database.insert database op;
  let open Signal in
  let a, b = input "a" num_bits, input "b" num_bits in
  let outputs = Combinational_op.instantiate op ~inputs:[ a; b ] in
  let outputs = List.map2_exn [ "c"; "d" ] outputs ~f:output in
  database, Circuit.create_exn ~name:"top" outputs
;;

let create_circuit_mutable = create_circuit create_op_mutable
let create_circuit = create_circuit create_op

let%expect_test "internal representation" =
  let _, circuit = create_circuit () in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module top (
        b,
        a,
        c,
        d
    );

        input [7:0] b;
        input [7:0] a;
        output [7:0] c;
        output [7:0] d;

        /* signal declarations */
        wire [7:0] _7;
        wire [15:0] _6;
        wire [7:0] _8;

        /* logic */
        assign _7 = _6[15:8];
        add_sub
            the_add_sub
            ( .i0(a), .i1(b), .o1(_6[15:8]), .o0(_6[7:0]) );
        assign _8 = _6[7:0];

        /* aliases */

        /* output assignments */
        assign c = _8;
        assign d = _7;

    endmodule |}]
;;

let%expect_test "internal representation (mutable)" =
  let _, circuit = create_circuit_mutable () in
  Rtl.print Verilog circuit;
  [%expect
    {|
    module top (
        b,
        a,
        c,
        d
    );

        input [7:0] b;
        input [7:0] a;
        output [7:0] c;
        output [7:0] d;

        /* signal declarations */
        wire [7:0] _7;
        wire [15:0] _6;
        wire [7:0] _8;

        /* logic */
        assign _7 = _6[15:8];
        add_sub_mutable
            the_add_sub_mutable
            ( .i0(a), .i1(b), .o1(_6[15:8]), .o0(_6[7:0]) );
        assign _8 = _6[7:0];

        /* aliases */

        /* output assignments */
        assign c = _8;
        assign d = _7;

    endmodule |}]
;;

let sim_functional ~database circuit =
  Cyclesim.create ~combinational_ops_database:database circuit
;;

let sim_imperative ~database circuit =
  Cyclesim.create ~combinational_ops_database:database circuit
;;

let testbench ~create_circuit ~create_sim =
  let database, circuit = create_circuit () in
  let sim = create_sim ~database circuit in
  let a, b = Cyclesim.in_port sim "a", Cyclesim.in_port sim "b" in
  let c, d = Cyclesim.out_port sim "c", Cyclesim.out_port sim "d" in
  List.init 4 ~f:(fun i ->
    List.init 4 ~f:(fun j ->
      a := Bits.of_int ~width:num_bits i;
      b := Bits.of_int ~width:num_bits j;
      Cyclesim.cycle sim;
      !a, !b, !c, !d))
  |> List.concat
;;

(* Check all variants are the same - functional / imperative simulation and [Bits] /
   [Bits.Mutable] operation implementations - including those derived automatically *)
let%expect_test "functional sim / bits" =
  let o1 = testbench ~create_circuit ~create_sim:sim_functional in
  let o2 = testbench ~create_circuit ~create_sim:sim_imperative in
  let o3 = testbench ~create_circuit:create_circuit_mutable ~create_sim:sim_functional in
  let o4 = testbench ~create_circuit:create_circuit_mutable ~create_sim:sim_imperative in
  let equal o o' =
    List.equal
      (fun (_, _, c1, c2) (_, _, c1', c2') -> Bits.equal c1 c1' && Bits.equal c2 c2')
      o
      o'
  in
  require [%here] (equal o1 o2);
  require [%here] (equal o1 o3);
  require [%here] (equal o1 o4);
  let sexp_of_result (a, b, c, d) =
    [%message "" (a : Bits.t) (b : Bits.t) (c : Bits.t) (d : Bits.t)]
  in
  print_s [%message "" ~results:(o1 : result list)];
  [%expect
    {|
    (results (
      ((a 00000000)
       (b 00000000)
       (c 00000000)
       (d 00000000))
      ((a 00000000)
       (b 00000001)
       (c 00000001)
       (d 11111111))
      ((a 00000000)
       (b 00000010)
       (c 00000010)
       (d 11111110))
      ((a 00000000)
       (b 00000011)
       (c 00000011)
       (d 11111101))
      ((a 00000001)
       (b 00000000)
       (c 00000001)
       (d 00000001))
      ((a 00000001)
       (b 00000001)
       (c 00000010)
       (d 00000000))
      ((a 00000001)
       (b 00000010)
       (c 00000011)
       (d 11111111))
      ((a 00000001)
       (b 00000011)
       (c 00000100)
       (d 11111110))
      ((a 00000010)
       (b 00000000)
       (c 00000010)
       (d 00000010))
      ((a 00000010)
       (b 00000001)
       (c 00000011)
       (d 00000001))
      ((a 00000010)
       (b 00000010)
       (c 00000100)
       (d 00000000))
      ((a 00000010)
       (b 00000011)
       (c 00000101)
       (d 11111111))
      ((a 00000011)
       (b 00000000)
       (c 00000011)
       (d 00000011))
      ((a 00000011)
       (b 00000001)
       (c 00000100)
       (d 00000010))
      ((a 00000011)
       (b 00000010)
       (c 00000101)
       (d 00000001))
      ((a 00000011)
       (b 00000011)
       (c 00000110)
       (d 00000000)))) |}]
;;
