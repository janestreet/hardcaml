open! Import

module Inner = struct
  module I = struct
    type 'a t =
      { a : 'a
      ; b : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { c : 'a
      ; d : 'a
      }
    [@@deriving hardcaml]
  end

  open Signal

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    { O.c = ~:(i.a) -- "a"; d = i.b }
  ;;
end

module Middle = struct
  module I = Inner.I

  module O = struct
    type 'a t =
      { o : 'a Inner.O.t array [@length 2]
      ; x : 'a
      }
    [@@deriving hardcaml]
  end

  module Inner_inst = Hierarchy.In_scope (Inner.I) (Inner.O)

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let (o1 : _ Inner.O.t) = Inner_inst.create ~scope ~name:"inner" Inner.create i in
    let (o2 : _ Inner.O.t) =
      let keep_hierarchy = Rtl_attribute.Vivado.keep_hierarchy true in
      Inner_inst.hierarchical
        ~attributes:[ keep_hierarchy ]
        ~scope
        ~name:"inner"
        Inner.create
        i
    in
    { O.o = [| o1; o2 |]; x = Signal.of_int ~width:1 0 -- "x" }
  ;;
end

module Outer = struct
  module I = Middle.I
  module O = Middle.O
  module Middle_inst = Hierarchy.In_scope (Middle.I) (Middle.O)

  let create scope (i : _ I.t) =
    Middle_inst.hierarchical ~scope ~instance:"the_middle" ~name:"middle" Middle.create i
  ;;
end

let%expect_test "flattened" =
  let name = "outer" in
  let module Circuit = Circuit.With_interface (Outer.I) (Outer.O) in
  let test (naming_scheme : Scope.Naming_scheme.t) =
    let scope = Scope.create ~flatten_design:true ~naming_scheme ~name () in
    Rtl.print
      ~database:(Scope.circuit_database scope)
      Verilog
      (Circuit.create_exn ~name (Outer.create scope))
  in
  test No_path;
  [%expect
    {|
    module outer (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire x_0;
        wire a_0;
        wire _5;
        wire _8;
        wire a_1;
        assign x_0 = 1'b0;
        assign a_0 = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign a_1 = ~ _8;
        assign c0 = a_1;
        assign d0 = _5;
        assign c1 = a_0;
        assign d1 = _5;
        assign x = x_0;

    endmodule
    |}];
  test Local_path;
  [%expect
    {|
    module outer (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire the_middle$x;
        wire inner_0$a;
        wire _5;
        wire _8;
        wire inner$a;
        assign the_middle$x = 1'b0;
        assign inner_0$a = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign inner$a = ~ _8;
        assign c0 = inner$a;
        assign d0 = _5;
        assign c1 = inner_0$a;
        assign d1 = _5;
        assign x = the_middle$x;

    endmodule
    |}];
  test Full_path;
  [%expect
    {|
    module outer (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire outer$the_middle$x;
        wire outer$the_middle$inner_0$a;
        wire _5;
        wire _8;
        wire outer$the_middle$inner$a;
        assign outer$the_middle$x = 1'b0;
        assign outer$the_middle$inner_0$a = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign outer$the_middle$inner$a = ~ _8;
        assign c0 = outer$the_middle$inner$a;
        assign d0 = _5;
        assign c1 = outer$the_middle$inner_0$a;
        assign d1 = _5;
        assign x = outer$the_middle$x;

    endmodule
    |}]
;;

let%expect_test "hierarchical" =
  let name = "outer" in
  let module Circuit = Circuit.With_interface (Outer.I) (Outer.O) in
  let test (naming_scheme : Scope.Naming_scheme.t) =
    let scope = Scope.create ~flatten_design:false ~naming_scheme ~name () in
    Rtl.print
      ~database:(Scope.circuit_database scope)
      Verilog
      (Circuit.create_exn ~name (Outer.create scope))
  in
  test No_path;
  [%expect
    {|
    module inner (
        b,
        a,
        c,
        d
    );

        input b;
        input a;
        output c;
        output d;

        wire _2;
        wire _5;
        wire a_0;
        assign _2 = b;
        assign _5 = a;
        assign a_0 = ~ _5;
        assign c = a_0;
        assign d = _2;

    endmodule
    module middle (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire x_0;
        wire _13;
        wire [1:0] _12;
        wire _14;
        wire _5;
        wire _8;
        wire a_0;
        assign x_0 = 1'b0;
        assign _13 = _12[1:1];
        (* keep_hierarchy="yes" *)
        inner
            inner_0
            ( .a(_8),
              .b(_5),
              .d(_12[1:1]),
              .c(_12[0:0]) );
        assign _14 = _12[0:0];
        assign _5 = b;
        assign _8 = a;
        assign a_0 = ~ _8;
        assign c0 = a_0;
        assign d0 = _5;
        assign c1 = _14;
        assign d1 = _13;
        assign x = x_0;

    endmodule
    module outer (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire _12;
        wire _13;
        wire _14;
        wire _15;
        wire _6;
        wire _8;
        wire [4:0] _11;
        wire _16;
        assign _12 = _11[4:4];
        assign _13 = _11[3:3];
        assign _14 = _11[2:2];
        assign _15 = _11[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8),
              .b(_6),
              .x(_11[4:4]),
              .d1(_11[3:3]),
              .c1(_11[2:2]),
              .d0(_11[1:1]),
              .c0(_11[0:0]) );
        assign _16 = _11[0:0];
        assign c0 = _16;
        assign d0 = _15;
        assign c1 = _14;
        assign d1 = _13;
        assign x = _12;

    endmodule
    |}];
  test Local_path;
  [%expect
    {|
    module inner (
        b,
        a,
        c,
        d
    );

        input b;
        input a;
        output c;
        output d;

        wire _2;
        wire _5;
        wire inner_0$a;
        assign _2 = b;
        assign _5 = a;
        assign inner_0$a = ~ _5;
        assign c = inner_0$a;
        assign d = _2;

    endmodule
    module middle (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire the_middle$x;
        wire _13;
        wire [1:0] _12;
        wire _14;
        wire _5;
        wire _8;
        wire inner$a;
        assign the_middle$x = 1'b0;
        assign _13 = _12[1:1];
        (* keep_hierarchy="yes" *)
        inner
            inner_0
            ( .a(_8),
              .b(_5),
              .d(_12[1:1]),
              .c(_12[0:0]) );
        assign _14 = _12[0:0];
        assign _5 = b;
        assign _8 = a;
        assign inner$a = ~ _8;
        assign c0 = inner$a;
        assign d0 = _5;
        assign c1 = _14;
        assign d1 = _13;
        assign x = the_middle$x;

    endmodule
    module outer (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire _12;
        wire _13;
        wire _14;
        wire _15;
        wire _6;
        wire _8;
        wire [4:0] _11;
        wire _16;
        assign _12 = _11[4:4];
        assign _13 = _11[3:3];
        assign _14 = _11[2:2];
        assign _15 = _11[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8),
              .b(_6),
              .x(_11[4:4]),
              .d1(_11[3:3]),
              .c1(_11[2:2]),
              .d0(_11[1:1]),
              .c0(_11[0:0]) );
        assign _16 = _11[0:0];
        assign c0 = _16;
        assign d0 = _15;
        assign c1 = _14;
        assign d1 = _13;
        assign x = _12;

    endmodule
    |}];
  test Full_path;
  [%expect
    {|
    module inner (
        b,
        a,
        c,
        d
    );

        input b;
        input a;
        output c;
        output d;

        wire _2;
        wire _5;
        wire outer$the_middle$inner_0$a;
        assign _2 = b;
        assign _5 = a;
        assign outer$the_middle$inner_0$a = ~ _5;
        assign c = outer$the_middle$inner_0$a;
        assign d = _2;

    endmodule
    module middle (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire outer$the_middle$x;
        wire _13;
        wire [1:0] _12;
        wire _14;
        wire _5;
        wire _8;
        wire outer$the_middle$inner$a;
        assign outer$the_middle$x = 1'b0;
        assign _13 = _12[1:1];
        (* keep_hierarchy="yes" *)
        inner
            inner_0
            ( .a(_8),
              .b(_5),
              .d(_12[1:1]),
              .c(_12[0:0]) );
        assign _14 = _12[0:0];
        assign _5 = b;
        assign _8 = a;
        assign outer$the_middle$inner$a = ~ _8;
        assign c0 = outer$the_middle$inner$a;
        assign d0 = _5;
        assign c1 = _14;
        assign d1 = _13;
        assign x = outer$the_middle$x;

    endmodule
    module outer (
        b,
        a,
        c0,
        d0,
        c1,
        d1,
        x
    );

        input b;
        input a;
        output c0;
        output d0;
        output c1;
        output d1;
        output x;

        wire _12;
        wire _13;
        wire _14;
        wire _15;
        wire _6;
        wire _8;
        wire [4:0] _11;
        wire _16;
        assign _12 = _11[4:4];
        assign _13 = _11[3:3];
        assign _14 = _11[2:2];
        assign _15 = _11[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8),
              .b(_6),
              .x(_11[4:4]),
              .d1(_11[3:3]),
              .c1(_11[2:2]),
              .d0(_11[1:1]),
              .c0(_11[0:0]) );
        assign _16 = _11[0:0];
        assign c0 = _16;
        assign d0 = _15;
        assign c1 = _14;
        assign d1 = _13;
        assign x = _12;

    endmodule
    |}]
;;

module Floating_inner = struct
  include Inner
  open Signal

  let create scope foo (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    { O.c = ~:(i.a) -- "a"; d = foo |: i.b }
  ;;

  let hierarchical scope foo =
    let module Hier = Hierarchy.In_scope (I) (O) in
    Hier.hierarchical ~scope ~name:"floating_inner" (fun scope i -> create scope foo i)
  ;;
end

module Floating_outer = struct
  module I = struct
    type 'a t =
      { foo : 'a
      ; inner : 'a Floating_inner.I.t
      }
    [@@deriving hardcaml]
  end

  module O = Floating_inner.O

  let create scope (i : _ I.t) = Floating_inner.hierarchical scope i.foo i.inner
end

let%expect_test "floating ports not in interface" =
  let module Circ = Circuit.With_interface (Floating_outer.I) (Floating_outer.O) in
  let scope = Scope.create ~flatten_design:false () in
  require_does_raise [%here] (fun () ->
    let circ =
      Circ.create_exn
        ~config:{ Circuit.Config.default with port_checks = Relaxed }
        ~name:"floating_outer"
        (Floating_outer.create scope)
    in
    Rtl.print Verilog ~database:(Scope.circuit_database scope) circ);
  [%expect
    {|
    ("Port sets do not match"
      (direction input)
      (expected_ports (a b))
      (actual_ports (a b foo))
      (expected_but_not_in_circuit ())
      (in_circuit_but_not_expected (foo))
      (circuit (
        (name floating_inner) (input_ports (b foo a)) (output_ports (c d)))))
    |}]
;;
