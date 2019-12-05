open! Import

module Inner = struct
  module I = struct
    type 'a t =
      { a : 'a
      ; b : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { c : 'a
      ; d : 'a
      }
    [@@deriving sexp_of, hardcaml]
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
    [@@deriving sexp_of, hardcaml]
  end

  module Inner_inst = Hierarchy.In_scope (Inner.I) (Inner.O)

  let create scope (i : _ I.t) =
    let ( -- ) = Scope.naming scope in
    let (o1 : _ Inner.O.t) = Inner_inst.create ~scope ~name:"inner" Inner.create i in
    let (o2 : _ Inner.O.t) =
      Inner_inst.hierarchical ~scope ~name:"inner" Inner.create i
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

          /* signal declarations */
          wire x_0 = 1'b0;
          wire a_0;
          wire _5;
          wire _8;
          wire a_1;

          /* logic */
          assign a_0 = ~ _8;
          assign _5 = b;
          assign _8 = a;
          assign a_1 = ~ _8;

          /* aliases */

          /* output assignments */
          assign c0 = a_1;
          assign d0 = _5;
          assign c1 = a_0;
          assign d1 = _5;
          assign x = x_0;

      endmodule |}];
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

        /* signal declarations */
        wire the_middle$x = 1'b0;
        wire inner$a;
        wire _5;
        wire _8;
        wire inner$a_0;

        /* logic */
        assign inner$a = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign inner$a_0 = ~ _8;

        /* aliases */

        /* output assignments */
        assign c0 = inner$a_0;
        assign d0 = _5;
        assign c1 = inner$a;
        assign d1 = _5;
        assign x = the_middle$x;

    endmodule |}];
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

        /* signal declarations */
        wire outer$the_middle$x = 1'b0;
        wire outer$the_middle$inner$a;
        wire _5;
        wire _8;
        wire outer$the_middle$inner$a_0;

        /* logic */
        assign outer$the_middle$inner$a = ~ _8;
        assign _5 = b;
        assign _8 = a;
        assign outer$the_middle$inner$a_0 = ~ _8;

        /* aliases */

        /* output assignments */
        assign c0 = outer$the_middle$inner$a_0;
        assign d0 = _5;
        assign c1 = outer$the_middle$inner$a;
        assign d1 = _5;
        assign x = outer$the_middle$x;

    endmodule |}]
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

          /* signal declarations */
          wire _2;
          wire _5;
          wire a_0;

          /* logic */
          assign _2 = b;
          assign _5 = a;
          assign a_0 = ~ _5;

          /* aliases */

          /* output assignments */
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

          /* signal declarations */
          wire x_0 = 1'b0;
          wire _13;
          wire [1:0] _12;
          wire _14;
          wire _5;
          wire _8;
          wire a_0;

          /* logic */
          assign _13 = _12[1:1];
          inner
              the_inner
              ( .a(_8), .b(_5), .d(_12[1:1]), .c(_12[0:0]) );
          assign _14 = _12[0:0];
          assign _5 = b;
          assign _8 = a;
          assign a_0 = ~ _8;

          /* aliases */

          /* output assignments */
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

          /* signal declarations */
          wire _12;
          wire _13;
          wire _14;
          wire _15;
          wire _6;
          wire _8;
          wire [4:0] _11;
          wire _16;

          /* logic */
          assign _12 = _11[4:4];
          assign _13 = _11[3:3];
          assign _14 = _11[2:2];
          assign _15 = _11[1:1];
          assign _6 = b;
          assign _8 = a;
          middle
              the_middle
              ( .a(_8), .b(_6), .x(_11[4:4]), .d1(_11[3:3]), .c1(_11[2:2]), .d0(_11[1:1]), .c0(_11[0:0]) );
          assign _16 = _11[0:0];

          /* aliases */

          /* output assignments */
          assign c0 = _16;
          assign d0 = _15;
          assign c1 = _14;
          assign d1 = _13;
          assign x = _12;

      endmodule |}];
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

        /* signal declarations */
        wire _2;
        wire _5;
        wire inner$a;

        /* logic */
        assign _2 = b;
        assign _5 = a;
        assign inner$a = ~ _5;

        /* aliases */

        /* output assignments */
        assign c = inner$a;
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

        /* signal declarations */
        wire the_middle$x = 1'b0;
        wire _13;
        wire [1:0] _12;
        wire _14;
        wire _5;
        wire _8;
        wire inner$a;

        /* logic */
        assign _13 = _12[1:1];
        inner
            the_inner
            ( .a(_8), .b(_5), .d(_12[1:1]), .c(_12[0:0]) );
        assign _14 = _12[0:0];
        assign _5 = b;
        assign _8 = a;
        assign inner$a = ~ _8;

        /* aliases */

        /* output assignments */
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

        /* signal declarations */
        wire _12;
        wire _13;
        wire _14;
        wire _15;
        wire _6;
        wire _8;
        wire [4:0] _11;
        wire _16;

        /* logic */
        assign _12 = _11[4:4];
        assign _13 = _11[3:3];
        assign _14 = _11[2:2];
        assign _15 = _11[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8), .b(_6), .x(_11[4:4]), .d1(_11[3:3]), .c1(_11[2:2]), .d0(_11[1:1]), .c0(_11[0:0]) );
        assign _16 = _11[0:0];

        /* aliases */

        /* output assignments */
        assign c0 = _16;
        assign d0 = _15;
        assign c1 = _14;
        assign d1 = _13;
        assign x = _12;

    endmodule |}];
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

        /* signal declarations */
        wire _2;
        wire _5;
        wire outer$the_middle$inner$a;

        /* logic */
        assign _2 = b;
        assign _5 = a;
        assign outer$the_middle$inner$a = ~ _5;

        /* aliases */

        /* output assignments */
        assign c = outer$the_middle$inner$a;
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

        /* signal declarations */
        wire outer$the_middle$x = 1'b0;
        wire _13;
        wire [1:0] _12;
        wire _14;
        wire _5;
        wire _8;
        wire outer$the_middle$inner$a;

        /* logic */
        assign _13 = _12[1:1];
        inner
            the_inner
            ( .a(_8), .b(_5), .d(_12[1:1]), .c(_12[0:0]) );
        assign _14 = _12[0:0];
        assign _5 = b;
        assign _8 = a;
        assign outer$the_middle$inner$a = ~ _8;

        /* aliases */

        /* output assignments */
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

        /* signal declarations */
        wire _12;
        wire _13;
        wire _14;
        wire _15;
        wire _6;
        wire _8;
        wire [4:0] _11;
        wire _16;

        /* logic */
        assign _12 = _11[4:4];
        assign _13 = _11[3:3];
        assign _14 = _11[2:2];
        assign _15 = _11[1:1];
        assign _6 = b;
        assign _8 = a;
        middle
            the_middle
            ( .a(_8), .b(_6), .x(_11[4:4]), .d1(_11[3:3]), .c1(_11[2:2]), .d0(_11[1:1]), .c0(_11[0:0]) );
        assign _16 = _11[0:0];

        /* aliases */

        /* output assignments */
        assign c0 = _16;
        assign d0 = _15;
        assign c1 = _14;
        assign d1 = _13;
        assign x = _12;

    endmodule |}]
;;
