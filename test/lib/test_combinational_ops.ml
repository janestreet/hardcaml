open! Import

let%expect_test "no outputs" =
  require_does_raise (fun () ->
    Combinational_op.create_mutable
      ()
      ~name:"no_outputs"
      ~input_widths:[]
      ~output_widths:[]
      ~create_fn:(fun ~inputs:_ ~outputs:_ -> Staged.stage Fn.id));
  [%expect {| "[Combinational_op]s require at least one output" |}]
;;

let%expect_test "no inputs" =
  require_does_not_raise (fun () ->
    ignore
      (Combinational_op.create_mutable
         ()
         ~name:"no_inputs"
         ~input_widths:[]
         ~output_widths:[ 1 ]
         ~create_fn:(fun ~inputs:_ ~outputs:_ -> Staged.stage Fn.id)
       : Combinational_op.t));
  [%expect {| |}]
;;

let%expect_test "bad output width" =
  require_does_raise (fun () ->
    Combinational_op.create_mutable
      ()
      ~name:"bad_width"
      ~input_widths:[ 1 ]
      ~output_widths:[ 0 ]
      ~create_fn:(fun ~inputs:_ ~outputs:_ -> Staged.stage Fn.id));
  [%expect {| ("[Combinational_op] output width <=0" (width 0)) |}]
;;

let%expect_test "bad input width" =
  require_does_raise (fun () ->
    Combinational_op.create_mutable
      ()
      ~name:"bad_width"
      ~input_widths:[ 0 ]
      ~output_widths:[ 1 ]
      ~create_fn:(fun ~inputs:_ ~outputs:_ -> Staged.stage Fn.id));
  [%expect {| ("[Combinational_op] input width <=0" (width 0)) |}]
;;

let%expect_test "names must be unique in database" =
  let op =
    Combinational_op.create_mutable
      ()
      ~name:"foo"
      ~input_widths:[ 1 ]
      ~output_widths:[ 1 ]
      ~create_fn:(fun ~inputs:_ ~outputs:_ -> Staged.stage Fn.id)
  in
  let database = Combinational_ops_database.create () in
  Combinational_ops_database.insert database op;
  require_does_raise (fun () -> Combinational_ops_database.insert database op);
  [%expect
    {|
    ("A [Combinational_op] of the same name already exists in the database"
     (name foo))
    |}]
;;

(* test add_sub; a 2 input, 2 output function *)

let num_bits = 8

let create_op_functional () =
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
  Combinational_op.create_mutable
    ()
    ~name:"add_sub_mutable"
    ~input_widths:[ num_bits; num_bits ]
    ~output_widths:[ num_bits; num_bits ]
    ~create_fn:(fun ~inputs ~outputs ->
      Staged.stage (fun () ->
        match inputs, outputs with
        | [ a; b ], [ c; d ] ->
          Bits.Mutable.( +: ) c a b;
          Bits.Mutable.( -: ) d a b
        | _ -> raise_s [%message "invalid arguments"]))
;;

let%expect_test "sexp_of" =
  let database = Combinational_ops_database.create () in
  Combinational_ops_database.insert database (create_op_functional ());
  Combinational_ops_database.insert database (create_op_mutable ());
  print_s [%sexp (database : Combinational_ops_database.t)];
  [%expect
    {|
    ((
      by_name (
        (add_sub (
          (name add_sub)
          (input_widths  (8 8))
          (output_widths (8 8))
          (create_fn <opaque>)))
        (add_sub_mutable (
          (name add_sub_mutable)
          (input_widths  (8 8))
          (output_widths (8 8))
          (create_fn <opaque>))))))
    |}]
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
let create_circuit = create_circuit create_op_functional

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

        wire [7:0] _6;
        wire [15:0] _5;
        wire [7:0] _7;
        assign _6 = _5[15:8];
        add_sub
            the_add_sub
            ( .i0(a),
              .i1(b),
              .o0(_5[7:0]),
              .o1(_5[15:8]) );
        assign _7 = _5[7:0];
        assign c = _7;
        assign d = _6;

    endmodule
    |}]
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

        wire [7:0] _6;
        wire [15:0] _5;
        wire [7:0] _7;
        assign _6 = _5[15:8];
        add_sub_mutable
            the_add_sub_mutable
            ( .i0(a),
              .i1(b),
              .o0(_5[7:0]),
              .o1(_5[15:8]) );
        assign _7 = _5[7:0];
        assign c = _7;
        assign d = _6;

    endmodule
    |}]
;;

let create_sim ~database circuit =
  Cyclesim.create
    ~config:{ Cyclesim.Config.default with combinational_ops_database = database }
    circuit
;;

let testbench ~create_circuit =
  let database, circuit = create_circuit () in
  let sim = create_sim ~database circuit in
  let a, b = Cyclesim.in_port sim "a", Cyclesim.in_port sim "b" in
  let c, d = Cyclesim.out_port sim "c", Cyclesim.out_port sim "d" in
  List.init 4 ~f:(fun i ->
    List.init 4 ~f:(fun j ->
      a := Bits.of_int_trunc ~width:num_bits i;
      b := Bits.of_int_trunc ~width:num_bits j;
      Cyclesim.cycle sim;
      !a, !b, !c, !d))
  |> List.concat
;;

(* Check all variants are the same - functional / imperative simulation and [Bits] /
   [Bits.Mutable] operation implementations - including those derived automatically *)
let%expect_test "functional sim / bits" =
  let o1 = testbench ~create_circuit in
  let o2 = testbench ~create_circuit:create_circuit_mutable in
  let equal o o' =
    List.equal
      (fun (_, _, c1, c2) (_, _, c1', c2') -> Bits.equal c1 c1' && Bits.equal c2 c2')
      o
      o'
  in
  require (equal o1 o2);
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
       (d 00000000))))
    |}]
;;

let%expect_test "instantiate with wrong input width" =
  let op =
    Combinational_op.create_mutable
      ()
      ~name:"invalid_inputs_width"
      ~input_widths:[ 3 ]
      ~output_widths:[ 1 ]
      ~create_fn:(fun ~inputs:_ ~outputs:_ -> Staged.stage Fn.id)
  in
  require_does_raise (fun () -> Combinational_op.instantiate op ~inputs:[ Signal.wire 4 ]);
  [%expect
    {|
    ("[Combinational_op.instantiate] input signal is wrong width"
     (t.name         invalid_inputs_width)
     (expected_width 3)
     (input_signal (wire (width 4))))
    |}]
;;

let%expect_test "outputs of ops with different widths" =
  let test w1 w2 =
    let op =
      Combinational_op.create_mutable
        ()
        ~name:"port_widths"
        ~input_widths:[ 3; 2 ]
        ~output_widths:[ w1; w2 ]
        ~create_fn:(fun ~inputs ~outputs ->
          let a, b, c, d =
            match inputs, outputs with
            | [ a; b ], [ c; d ] -> a, b, c, d
            | _ -> raise_s [%message "invalid arguments"]
          in
          Staged.stage (fun () ->
            Bits.Mutable.copy_bits
              ~dst:c
              ~src:
                (Bits.of_unsigned_int
                   ~width:(Bits.Mutable.width c)
                   (Bits.Mutable.width a));
            Bits.Mutable.copy_bits
              ~dst:d
              ~src:
                (Bits.of_unsigned_int
                   ~width:(Bits.Mutable.width d)
                   (Bits.Mutable.width b));
            print_s
              [%message
                (Bits.Mutable.to_bits c : Bits.t)
                  (Bits.Mutable.to_bits d : Bits.t)
                  (Bits.Mutable.width a : int)
                  (Bits.Mutable.width b : int)
                  (Bits.Mutable.width c : int)
                  (Bits.Mutable.width d : int)]))
    in
    let database = Combinational_ops_database.create () in
    Combinational_ops_database.insert database op;
    let open Signal in
    let a, b = input "a" 3, input "b" 2 in
    let outputs = Combinational_op.instantiate op ~inputs:[ a; b ] in
    let outputs = List.map2_exn [ "c"; "d" ] outputs ~f:output in
    let circuit = Circuit.create_exn ~name:"top" outputs in
    let sim = create_sim ~database circuit in
    let c, d = Cyclesim.out_port sim "c", Cyclesim.out_port sim "d" in
    Cyclesim.cycle sim;
    print_s [%message (c : Bits.t ref) (d : Bits.t ref)]
  in
  test 4 4;
  [%expect
    {|
    (("Bits.Mutable.to_bits c" 0011)
     ("Bits.Mutable.to_bits d" 0010)
     ("Bits.Mutable.width a"   3)
     ("Bits.Mutable.width b"   2)
     ("Bits.Mutable.width c"   4)
     ("Bits.Mutable.width d"   4))
    ((c 0011)
     (d 0010))
    |}];
  test 3 4;
  [%expect
    {|
    (("Bits.Mutable.to_bits c" 011)
     ("Bits.Mutable.to_bits d" 0010)
     ("Bits.Mutable.width a"   3)
     ("Bits.Mutable.width b"   2)
     ("Bits.Mutable.width c"   3)
     ("Bits.Mutable.width d"   4))
    ((c 011)
     (d 0010))
    |}];
  test 7 5;
  [%expect
    {|
    (("Bits.Mutable.to_bits c" 0000011)
     ("Bits.Mutable.to_bits d" 00010)
     ("Bits.Mutable.width a"   3)
     ("Bits.Mutable.width b"   2)
     ("Bits.Mutable.width c"   7)
     ("Bits.Mutable.width d"   5))
    ((c 0000011)
     (d 00010))
    |}]
;;

module I = struct
  type 'a t =
    { a : 'a [@bits 4]
    ; b : 'a [@bits 5]
    ; c : 'a [@bits 6]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { x : 'a [@bits 6]
    ; y : 'a [@bits 5]
    ; z : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

let create ~database (i : _ I.t) =
  let module Op = Combinational_op.With_interface (I) (O) in
  let op =
    Op.create
      ~name:"interfaces"
      ~create_fn:(fun { a; b; c } -> { x = c; y = b; z = a })
      ()
  in
  Combinational_ops_database.insert database op;
  Op.instantiate op i
;;

let%expect_test "with interface" =
  let database = Combinational_ops_database.create () in
  let create = create ~database in
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim =
    Sim.create
      ~config:{ Cyclesim.Config.default with combinational_ops_database = database }
      create
  in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let open Bits in
  inputs.a <--. 3;
  inputs.b <--. 4;
  inputs.c <--. 5;
  Cyclesim.cycle sim;
  print_s
    [%message
      (outputs.x : Bits.Hex.t ref)
        (outputs.y : Bits.Hex.t ref)
        (outputs.z : Bits.Hex.t ref)];
  [%expect
    {|
    ((outputs.x 6'h05)
     (outputs.y 5'h04)
     (outputs.z 4'h3))
    |}]
;;
