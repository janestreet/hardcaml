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
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module Inner_inst = Hierarchy.In_scope (Inner.I) (Inner.O)

  let create ?(with_hierarchical_here = false) scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let (o1 : _ Inner.O.t) =
      Inner_inst.hierarchical
        ~how_to_instantiate:Inlined_in_scope
        ~scope
        ~name:"inner"
        Inner.create
        i
    in
    let (o2 : _ Inner.O.t) =
      let keep_hierarchy = Rtl_attribute.Vivado.keep_hierarchy true in
      if with_hierarchical_here
      then Inner_inst.hierarchical ~attributes:[ keep_hierarchy ] ~scope Inner.create i
      else
        Inner_inst.hierarchical
          ~attributes:[ keep_hierarchy ]
          ~scope
          ~name:"inner"
          Inner.create
          i
    in
    { O.o = [| o1; o2 |]; x = Signal.of_int_trunc ~width:1 0 -- "x" }
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
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
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
        assign c_0 = a_1;
        assign d_0 = _5;
        assign c_1 = a_0;
        assign d_1 = _5;
        assign x = x_0;

    endmodule
    |}];
  test Local_path;
  [%expect
    {|
    module outer (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire the_middle$x;
        wire inner_1$a;
        wire _5;
        wire _8;
        wire inner$a;
        assign the_middle$x = 1'b0;
        assign inner_1$a = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign inner$a = ~ _8;
        assign c_0 = inner$a;
        assign d_0 = _5;
        assign c_1 = inner_1$a;
        assign d_1 = _5;
        assign x = the_middle$x;

    endmodule
    |}];
  test Full_path;
  [%expect
    {|
    module outer (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire outer$the_middle$x;
        wire outer$the_middle$inner_1$a;
        wire _5;
        wire _8;
        wire outer$the_middle$inner$a;
        assign outer$the_middle$x = 1'b0;
        assign outer$the_middle$inner_1$a = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign outer$the_middle$inner$a = ~ _8;
        assign c_0 = outer$the_middle$inner$a;
        assign d_0 = _5;
        assign c_1 = outer$the_middle$inner_1$a;
        assign d_1 = _5;
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
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire x_0;
        wire _12;
        wire [1:0] _11;
        wire _13;
        wire _5;
        wire _8;
        wire a_0;
        assign x_0 = 1'b0;
        assign _12 = _11[1:1];
        (* keep_hierarchy="yes" *)
        inner
            inner_1
            ( .a(_8),
              .b(_5),
              .c(_11[0:0]),
              .d(_11[1:1]) );
        assign _13 = _11[0:0];
        assign _5 = b;
        assign _8 = a;
        assign a_0 = ~ _8;
        assign c_0 = a_0;
        assign d_0 = _5;
        assign c_1 = _13;
        assign d_1 = _12;
        assign x = x_0;

    endmodule
    module outer (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire _11;
        wire _12;
        wire _13;
        wire _14;
        wire _6;
        wire _8;
        wire [4:0] _10;
        wire _15;
        assign _11 = _10[4:4];
        assign _12 = _10[3:3];
        assign _13 = _10[2:2];
        assign _14 = _10[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8),
              .b(_6),
              .c_0(_10[0:0]),
              .d_0(_10[1:1]),
              .c_1(_10[2:2]),
              .d_1(_10[3:3]),
              .x(_10[4:4]) );
        assign _15 = _10[0:0];
        assign c_0 = _15;
        assign d_0 = _14;
        assign c_1 = _13;
        assign d_1 = _12;
        assign x = _11;

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
        wire inner_1$a;
        assign _2 = b;
        assign _5 = a;
        assign inner_1$a = ~ _5;
        assign c = inner_1$a;
        assign d = _2;

    endmodule
    module middle (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire the_middle$x;
        wire _12;
        wire [1:0] _11;
        wire _13;
        wire _5;
        wire _8;
        wire inner$a;
        assign the_middle$x = 1'b0;
        assign _12 = _11[1:1];
        (* keep_hierarchy="yes" *)
        inner
            inner_1
            ( .a(_8),
              .b(_5),
              .c(_11[0:0]),
              .d(_11[1:1]) );
        assign _13 = _11[0:0];
        assign _5 = b;
        assign _8 = a;
        assign inner$a = ~ _8;
        assign c_0 = inner$a;
        assign d_0 = _5;
        assign c_1 = _13;
        assign d_1 = _12;
        assign x = the_middle$x;

    endmodule
    module outer (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire _11;
        wire _12;
        wire _13;
        wire _14;
        wire _6;
        wire _8;
        wire [4:0] _10;
        wire _15;
        assign _11 = _10[4:4];
        assign _12 = _10[3:3];
        assign _13 = _10[2:2];
        assign _14 = _10[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8),
              .b(_6),
              .c_0(_10[0:0]),
              .d_0(_10[1:1]),
              .c_1(_10[2:2]),
              .d_1(_10[3:3]),
              .x(_10[4:4]) );
        assign _15 = _10[0:0];
        assign c_0 = _15;
        assign d_0 = _14;
        assign c_1 = _13;
        assign d_1 = _12;
        assign x = _11;

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
        wire outer$the_middle$inner_1$a;
        assign _2 = b;
        assign _5 = a;
        assign outer$the_middle$inner_1$a = ~ _5;
        assign c = outer$the_middle$inner_1$a;
        assign d = _2;

    endmodule
    module middle (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire outer$the_middle$x;
        wire _12;
        wire [1:0] _11;
        wire _13;
        wire _5;
        wire _8;
        wire outer$the_middle$inner$a;
        assign outer$the_middle$x = 1'b0;
        assign _12 = _11[1:1];
        (* keep_hierarchy="yes" *)
        inner
            inner_1
            ( .a(_8),
              .b(_5),
              .c(_11[0:0]),
              .d(_11[1:1]) );
        assign _13 = _11[0:0];
        assign _5 = b;
        assign _8 = a;
        assign outer$the_middle$inner$a = ~ _8;
        assign c_0 = outer$the_middle$inner$a;
        assign d_0 = _5;
        assign c_1 = _13;
        assign d_1 = _12;
        assign x = outer$the_middle$x;

    endmodule
    module outer (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire _11;
        wire _12;
        wire _13;
        wire _14;
        wire _6;
        wire _8;
        wire [4:0] _10;
        wire _15;
        assign _11 = _10[4:4];
        assign _12 = _10[3:3];
        assign _13 = _10[2:2];
        assign _14 = _10[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8),
              .b(_6),
              .c_0(_10[0:0]),
              .d_0(_10[1:1]),
              .c_1(_10[2:2]),
              .d_1(_10[3:3]),
              .x(_10[4:4]) );
        assign _15 = _10[0:0];
        assign c_0 = _15;
        assign d_0 = _14;
        assign c_1 = _13;
        assign d_1 = _12;
        assign x = _11;

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
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = Floating_inner.O

  let create scope (i : _ I.t) = Floating_inner.hierarchical scope i.foo i.inner
end

let%expect_test "floating ports not in interface" =
  let module Circ = Circuit.With_interface (Floating_outer.I) (Floating_outer.O) in
  let scope = Scope.create ~flatten_design:false () in
  require_does_raise (fun () ->
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

let%expect_test "[hierarchical_here] uses the file name as the module name" =
  let module Circuit = Circuit.With_interface (Middle.I) (Middle.O) in
  let scope = Scope.create ~flatten_design:true ~naming_scheme:Full_path () in
  Rtl.print
    ~database:(Scope.circuit_database scope)
    Verilog
    (Circuit.create_exn
       ~name:"circuit"
       (Middle.create ~with_hierarchical_here:true scope));
  (* Module name "test_module_hierarchy" comes from this file's name *)
  [%expect
    {|
    module circuit (
        b,
        a,
        c_0,
        d_0,
        c_1,
        d_1,
        x
    );

        input b;
        input a;
        output c_0;
        output d_0;
        output c_1;
        output d_1;
        output x;

        wire x_0;
        wire test_module_hierarchy$a;
        wire _5;
        wire _8;
        wire inner$a;
        assign x_0 = 1'b0;
        assign test_module_hierarchy$a = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign inner$a = ~ _8;
        assign c_0 = inner$a;
        assign d_0 = _5;
        assign c_1 = test_module_hierarchy$a;
        assign d_1 = _5;
        assign x = x_0;

    endmodule
    |}]
;;
