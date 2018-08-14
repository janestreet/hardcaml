open! Import

module Inner = struct
  module I = struct
    type 'a t =
      { a : 'a
      ; b : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  module O = struct
    type 'a t =
      { c : 'a
      ; d : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  open Signal
  let create scope (i : _ I.t) =
    let (--) = Scope.naming scope in
    { O.c = (~: (i.a)) -- "a"
    ; d = i.b }
end

module Middle = struct
  module I = Inner.I

  module O = struct
    type 'a t =
      { o : 'a Inner.O.t array[@length 2]
      ; x : 'a }
    [@@deriving sexp_of, hardcaml]
  end

  module Inner_inst = Hierarchy.In_scope (Inner.I) (Inner.O)

  let create scope (i : _ I.t) =
    let (--) = Scope.naming scope in
    let (o1 : _ Inner.O.t) = Inner_inst.create ~scope ~name:"inner" Inner.create i in
    let (o2 : _ Inner.O.t) = Inner_inst.hierarchical ~scope ~name:"inner" Inner.create i in
    { O.o = [| o1; o2; |]
    ; x = Signal.consti 1 0 -- "x" }
end

module Outer = struct
  module I = Middle.I
  module O = Middle.O

  module Middle_inst = Hierarchy.In_scope (Middle.I) (Middle.O)

  let create scope (i : _ I.t) =
    Middle_inst.hierarchical ~scope ~instance:"the_middle" ~name:"middle" Middle.create i
end

let%expect_test "flattened" =
  let name = "outer" in
  let module Circuit = Circuit.With_interface (Outer.I) (Outer.O) in
  let test (naming_scheme : Scope.Naming_scheme.t) =
    let scope = Scope.create ~flatten_design:true ~naming_scheme ~name () in
    Rtl.print ~database:(Scope.circuit_database scope) Verilog
      (Circuit.create_exn ~name (Outer.create scope))
  in
  test No_path;
  [%expect {|
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
          wire a_1;

          /* logic */
          assign a_0 = ~ a;
          assign a_1 = ~ a;

          /* aliases */

          /* output assignments */
          assign c0 = a_1;
          assign d0 = b;
          assign c1 = a_0;
          assign d1 = b;
          assign x = x_0;

      endmodule |}];
  test Local_path;
  [%expect {|
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
        wire inner$a_0;

        /* logic */
        assign inner$a = ~ a;
        assign inner$a_0 = ~ a;

        /* aliases */

        /* output assignments */
        assign c0 = inner$a_0;
        assign d0 = b;
        assign c1 = inner$a;
        assign d1 = b;
        assign x = the_middle$x;

    endmodule |}];
  test Full_path;
  [%expect {|
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
        wire outer$the_middle$inner$a_0;

        /* logic */
        assign outer$the_middle$inner$a = ~ a;
        assign outer$the_middle$inner$a_0 = ~ a;

        /* aliases */

        /* output assignments */
        assign c0 = outer$the_middle$inner$a_0;
        assign d0 = b;
        assign c1 = outer$the_middle$inner$a;
        assign d1 = b;
        assign x = outer$the_middle$x;

    endmodule |}]
;;

let%expect_test "hierarchical" =
  let name = "outer" in
  let module Circuit = Circuit.With_interface (Outer.I) (Outer.O) in
  let test (naming_scheme : Scope.Naming_scheme.t)  =
    let scope = Scope.create ~flatten_design:false ~naming_scheme ~name () in
    Rtl.print ~database:(Scope.circuit_database scope) Verilog
      (Circuit.create_exn ~name (Outer.create scope))
  in
  test No_path;
  [%expect {|
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
          wire a_0;

          /* logic */
          assign a_0 = ~ a;

          /* aliases */

          /* output assignments */
          assign c = a_0;
          assign d = b;

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
          wire _11;
          wire [1:0] _10;
          wire _12;
          wire a_0;

          /* logic */
          assign _11 = _10[1:1];
          inner
              the_inner
              ( .a(a), .b(b), .d(_10[1:1]), .c(_10[0:0]) );
          assign _12 = _10[0:0];
          assign a_0 = ~ a;

          /* aliases */

          /* output assignments */
          assign c0 = a_0;
          assign d0 = b;
          assign c1 = _12;
          assign d1 = _11;
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
          wire _10;
          wire _11;
          wire _12;
          wire _13;
          wire [4:0] _9;
          wire _14;

          /* logic */
          assign _10 = _9[4:4];
          assign _11 = _9[3:3];
          assign _12 = _9[2:2];
          assign _13 = _9[1:1];
          middle
              the_middle
              ( .a(a), .b(b), .x(_9[4:4]), .d1(_9[3:3]), .c1(_9[2:2]), .d0(_9[1:1]), .c0(_9[0:0]) );
          assign _14 = _9[0:0];

          /* aliases */

          /* output assignments */
          assign c0 = _14;
          assign d0 = _13;
          assign c1 = _12;
          assign d1 = _11;
          assign x = _10;

      endmodule |}];
  test Local_path;
  [%expect {|
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
        wire inner$a;

        /* logic */
        assign inner$a = ~ a;

        /* aliases */

        /* output assignments */
        assign c = inner$a;
        assign d = b;

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
        wire _11;
        wire [1:0] _10;
        wire _12;
        wire inner$a;

        /* logic */
        assign _11 = _10[1:1];
        inner
            the_inner
            ( .a(a), .b(b), .d(_10[1:1]), .c(_10[0:0]) );
        assign _12 = _10[0:0];
        assign inner$a = ~ a;

        /* aliases */

        /* output assignments */
        assign c0 = inner$a;
        assign d0 = b;
        assign c1 = _12;
        assign d1 = _11;
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
        wire _10;
        wire _11;
        wire _12;
        wire _13;
        wire [4:0] _9;
        wire _14;

        /* logic */
        assign _10 = _9[4:4];
        assign _11 = _9[3:3];
        assign _12 = _9[2:2];
        assign _13 = _9[1:1];
        middle
            the_middle
            ( .a(a), .b(b), .x(_9[4:4]), .d1(_9[3:3]), .c1(_9[2:2]), .d0(_9[1:1]), .c0(_9[0:0]) );
        assign _14 = _9[0:0];

        /* aliases */

        /* output assignments */
        assign c0 = _14;
        assign d0 = _13;
        assign c1 = _12;
        assign d1 = _11;
        assign x = _10;

    endmodule |}];
  test Full_path;
  [%expect {|
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
        wire outer$the_middle$inner$a;

        /* logic */
        assign outer$the_middle$inner$a = ~ a;

        /* aliases */

        /* output assignments */
        assign c = outer$the_middle$inner$a;
        assign d = b;

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
        wire _11;
        wire [1:0] _10;
        wire _12;
        wire outer$the_middle$inner$a;

        /* logic */
        assign _11 = _10[1:1];
        inner
            the_inner
            ( .a(a), .b(b), .d(_10[1:1]), .c(_10[0:0]) );
        assign _12 = _10[0:0];
        assign outer$the_middle$inner$a = ~ a;

        /* aliases */

        /* output assignments */
        assign c0 = outer$the_middle$inner$a;
        assign d0 = b;
        assign c1 = _12;
        assign d1 = _11;
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
        wire _10;
        wire _11;
        wire _12;
        wire _13;
        wire [4:0] _9;
        wire _14;

        /* logic */
        assign _10 = _9[4:4];
        assign _11 = _9[3:3];
        assign _12 = _9[2:2];
        assign _13 = _9[1:1];
        middle
            the_middle
            ( .a(a), .b(b), .x(_9[4:4]), .d1(_9[3:3]), .c1(_9[2:2]), .d0(_9[1:1]), .c0(_9[0:0]) );
        assign _14 = _9[0:0];

        /* aliases */

        /* output assignments */
        assign c0 = _14;
        assign d0 = _13;
        assign c1 = _12;
        assign d1 = _11;
        assign x = _10;

    endmodule |}]
;;
