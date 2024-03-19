open Hardcaml

(* We cannot compile these examples unless we also compile their dependancies. Which would
   mean building a work library in a separate directory (otherwise parallel builds clash)
   and building black boxes for instantiations. *)

let%expect_test "instantiation, with 0 or more parameters." =
  let module I = struct
    type 'a t =
      { foo : 'a
      ; bar : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module O = struct
    type 'a t =
      { zoo : 'a
      ; moo : 'a [@bits 2]
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"temp" (fun (i : _ I.t) ->
      let module I = Instantiation.With_interface (I) (O) in
      let o1 = I.create ~name:"foo" i in
      let o2 =
        I.create ~parameters:[ Parameter.create ~name:"par" ~value:(Int 3) ] ~name:"foo" i
      in
      let o3 =
        I.create
          ~parameters:
            [ Parameter.create ~name:"par" ~value:(Int 3)
            ; Parameter.create ~name:"far" ~value:(String "baloo")
            ]
          ~name:"foo"
          i
      in
      O.map3 o1 o2 o3 ~f:Signal.(fun a b c -> a |: b |: c))
  in
  Testing.analyse_vhdl_and_verilog ~quiet:true ~show:true circuit;
  [%expect
    {|
    ("Icarus Verilog failed with" (error_code (Error (Exit_non_zero 4))))
    module temp (
        bar,
        foo,
        zoo,
        moo
    );

        input bar;
        input foo;
        output zoo;
        output [1:0] moo;

        wire [1:0] _16;
        wire [1:0] _12;
        wire [1:0] _9;
        wire [1:0] _13;
        wire [1:0] _17;
        wire [2:0] _15;
        wire _21;
        wire [2:0] _11;
        wire _19;
        wire _3;
        wire _5;
        wire [2:0] _8;
        wire _18;
        wire _20;
        wire _22;
        assign _16 = _15[2:1];
        assign _12 = _11[2:1];
        assign _9 = _8[2:1];
        assign _13 = _9 | _12;
        assign _17 = _13 | _16;
        foo
            #( .par(3),
               .far("baloo") )
            the_foo
            ( .foo(_5),
              .bar(_3),
              .moo(_15[2:1]),
              .zoo(_15[0:0]) );
        assign _21 = _15[0:0];
        foo
            #( .par(3) )
            the_foo_0
            ( .foo(_5),
              .bar(_3),
              .moo(_11[2:1]),
              .zoo(_11[0:0]) );
        assign _19 = _11[0:0];
        assign _3 = bar;
        assign _5 = foo;
        foo
            the_foo_1
            ( .foo(_5),
              .bar(_3),
              .moo(_8[2:1]),
              .zoo(_8[0:0]) );
        assign _18 = _8[0:0];
        assign _20 = _18 | _19;
        assign _22 = _20 | _21;
        assign zoo = _22;
        assign moo = _17;

    endmodule
    ("GHDL failed with" (error_code (Error (Exit_non_zero 1))))
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity temp is
        port (
            bar : in std_logic;
            foo : in std_logic;
            zoo : out std_logic;
            moo : out std_logic_vector(1 downto 0)
        );
    end entity;

    architecture rtl of temp is

        -- conversion functions
        function hc_uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;
        function hc_uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;
        function hc_sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;
        function hc_sgn(a : std_logic_vector) return signed           is begin return signed(a); end;
        function hc_sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;
        function hc_sl (a : unsigned)         return std_logic        is begin return a(a'right); end;
        function hc_sl (a : signed)           return std_logic        is begin return a(a'right); end;
        function hc_sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;
        function hc_slv(a : std_logic_vector) return std_logic_vector is begin return a; end;
        function hc_slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;
        function hc_slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;
        signal hc_16 : std_logic_vector(1 downto 0);
        signal hc_12 : std_logic_vector(1 downto 0);
        signal hc_9 : std_logic_vector(1 downto 0);
        signal hc_13 : std_logic_vector(1 downto 0);
        signal hc_17 : std_logic_vector(1 downto 0);
        signal hc_15 : std_logic_vector(2 downto 0);
        signal hc_21 : std_logic;
        signal hc_11 : std_logic_vector(2 downto 0);
        signal hc_19 : std_logic;
        signal hc_3 : std_logic;
        signal hc_5 : std_logic;
        signal hc_8 : std_logic_vector(2 downto 0);
        signal hc_18 : std_logic;
        signal hc_20 : std_logic;
        signal hc_22 : std_logic;

    begin

        hc_16 <= hc_15(2 downto 1);
        hc_12 <= hc_11(2 downto 1);
        hc_9 <= hc_8(2 downto 1);
        hc_13 <= hc_slv(hc_uns(hc_9) or hc_uns(hc_12));
        hc_17 <= hc_slv(hc_uns(hc_13) or hc_uns(hc_16));
        the_foo: entity work.foo (rtl)
            generic map ( par => 3,
                          far => "baloo" )
            port map ( foo => hc_5,
                       bar => hc_3,
                       moo => hc_15(2 downto 1),
                       zoo => hc_15(0) );
        hc_21 <= hc_sl(hc_15(0 downto 0));
        the_foo_0: entity work.foo (rtl)
            generic map ( par => 3 )
            port map ( foo => hc_5,
                       bar => hc_3,
                       moo => hc_11(2 downto 1),
                       zoo => hc_11(0) );
        hc_19 <= hc_sl(hc_11(0 downto 0));
        hc_3 <= bar;
        hc_5 <= foo;
        the_foo_1: entity work.foo (rtl)
            port map ( foo => hc_5,
                       bar => hc_3,
                       moo => hc_8(2 downto 1),
                       zoo => hc_8(0) );
        hc_18 <= hc_sl(hc_8(0 downto 0));
        hc_20 <= hc_sl(hc_uns(hc_18) or hc_uns(hc_19));
        hc_22 <= hc_sl(hc_uns(hc_20) or hc_uns(hc_21));
        zoo <= hc_22;
        moo <= hc_17;

    end architecture;
    |}]
;;

let%expect_test "instantiation output corner case" =
  let module I = struct
    type 'a t =
      { foo : 'a
      ; bar : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module O = struct
    type 'a t = { zoo : 'a } [@@deriving sexp_of, hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"temp" (fun (i : _ I.t) ->
      let module I = Instantiation.With_interface (I) (O) in
      I.create ~name:"foo" i)
  in
  Testing.analyse_vhdl_and_verilog ~quiet:true ~show:true circuit;
  [%expect
    {|
    ("Icarus Verilog failed with" (error_code (Error (Exit_non_zero 2))))
    module temp (
        bar,
        foo,
        zoo
    );

        input bar;
        input foo;
        output zoo;

        wire _2;
        wire _4;
        wire _8;
        wire _5;
        assign _2 = bar;
        assign _4 = foo;
        foo
            the_foo
            ( .foo(_4),
              .bar(_2),
              .zoo(_8) );
        assign _5 = _8;
        assign zoo = _5;

    endmodule
    ("GHDL failed with" (error_code (Error (Exit_non_zero 1))))
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity temp is
        port (
            bar : in std_logic;
            foo : in std_logic;
            zoo : out std_logic
        );
    end entity;

    architecture rtl of temp is

        -- conversion functions
        function hc_uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;
        function hc_uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;
        function hc_sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;
        function hc_sgn(a : std_logic_vector) return signed           is begin return signed(a); end;
        function hc_sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;
        function hc_sl (a : unsigned)         return std_logic        is begin return a(a'right); end;
        function hc_sl (a : signed)           return std_logic        is begin return a(a'right); end;
        function hc_sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;
        function hc_slv(a : std_logic_vector) return std_logic_vector is begin return a; end;
        function hc_slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;
        function hc_slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;
        signal hc_2 : std_logic;
        signal hc_4 : std_logic;
        signal hc_8 : std_logic;
        signal hc_5 : std_logic;

    begin

        hc_2 <= bar;
        hc_4 <= foo;
        the_foo: entity work.foo (rtl)
            port map ( foo => hc_4,
                       bar => hc_2,
                       zoo => hc_8 );
        hc_5 <= hc_8;
        zoo <= hc_5;

    end architecture;
    |}]
;;

let%expect_test "all parameter types" =
  let module I = struct
    type 'a t =
      { foo : 'a
      ; bar : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module O = struct
    type 'a t = { zoo : 'a } [@@deriving sexp_of, hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"temp" (fun (i : _ I.t) ->
      let module I = Instantiation.With_interface (I) (O) in
      I.create
        ~name:"foo"
        ~parameters:
          [ Parameter.create ~name:"a" ~value:(Bit true)
          ; Parameter.create ~name:"a2" ~value:(Bit false)
          ; Parameter.create ~name:"b" ~value:(Bit_vector [ 1; 0 ])
          ; Parameter.create ~name:"c" ~value:(Bool true)
          ; Parameter.create ~name:"c2" ~value:(Bool false)
          ; Parameter.create ~name:"d" ~value:(Int 123)
          ; Parameter.create ~name:"e" ~value:(Real 1.24)
          ; Parameter.create ~name:"f" ~value:(Std_logic U)
          ; Parameter.create
              ~name:"g"
              ~value:(Std_logic_vector [ U; X; L0; L1; Z; W; L; H; Don't_care ])
          ; Parameter.create ~name:"h" ~value:(Std_ulogic W)
          ; Parameter.create
              ~name:"i"
              ~value:(Std_ulogic_vector [ U; X; L0; L1; Z; W; L; H; Don't_care ])
          ; Parameter.create ~name:"j" ~value:(String "foo")
          ]
        i)
  in
  Testing.analyse_vhdl_and_verilog ~quiet:true ~show:true circuit;
  [%expect
    {|
    ("Icarus Verilog failed with" (error_code (Error (Exit_non_zero 3))))
    module temp (
        bar,
        foo,
        zoo
    );

        input bar;
        input foo;
        output zoo;

        wire _2;
        wire _4;
        wire _8;
        wire _5;
        assign _2 = bar;
        assign _4 = foo;
        foo
            #( .a(1'b1),
               .a2(1'b0),
               .b(2'b10),
               .c(1'b1),
               .c2(1'b0),
               .d(123),
               .e(1.240000),
               .f(4'd0),
               .g(9'bUX01ZWLH_),
               .h(4'd5),
               .i(9'bUX01ZWLH_),
               .j("foo") )
            the_foo
            ( .foo(_4),
              .bar(_2),
              .zoo(_8) );
        assign _5 = _8;
        assign zoo = _5;

    endmodule
    ("GHDL failed with" (error_code (Error (Exit_non_zero 1))))
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity temp is
        port (
            bar : in std_logic;
            foo : in std_logic;
            zoo : out std_logic
        );
    end entity;

    architecture rtl of temp is

        -- conversion functions
        function hc_uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;
        function hc_uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;
        function hc_sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;
        function hc_sgn(a : std_logic_vector) return signed           is begin return signed(a); end;
        function hc_sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;
        function hc_sl (a : unsigned)         return std_logic        is begin return a(a'right); end;
        function hc_sl (a : signed)           return std_logic        is begin return a(a'right); end;
        function hc_sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;
        function hc_slv(a : std_logic_vector) return std_logic_vector is begin return a; end;
        function hc_slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;
        function hc_slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;
        signal hc_2 : std_logic;
        signal hc_4 : std_logic;
        signal hc_8 : std_logic;
        signal hc_5 : std_logic;

    begin

        hc_2 <= bar;
        hc_4 <= foo;
        the_foo: entity work.foo (rtl)
            generic map ( a => '1',
                          a2 => '0',
                          b => "10",
                          c => true,
                          c2 => false,
                          d => 123,
                          e => 1.240000,
                          f => 'U',
                          g => std_logic_vector'("UX01ZWLH_"),
                          h => 'W',
                          i => std_ulogic_vector'("UX01ZWLH_"),
                          j => "foo" )
            port map ( foo => hc_4,
                       bar => hc_2,
                       zoo => hc_8 );
        hc_5 <= hc_8;
        zoo <= hc_5;

    end architecture;
    |}]
;;

let%expect_test "phantom input" =
  let module I = struct
    type 'a t =
      { foo : 'a
      ; bar : 'a
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module O = struct
    type 'a t = { zoo : 'a } [@@deriving sexp_of, hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit = C.create_exn ~name:"temp" (fun (i : _ I.t) -> { O.zoo = i.foo }) in
  Testing.analyse_vhdl_and_verilog ~quiet:true ~show:true circuit;
  [%expect
    {|
    module temp (
        foo,
        bar,
        zoo
    );

        input foo;
        input bar;
        output zoo;

        wire _2;
        assign _2 = foo;
        assign zoo = _2;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity temp is
        port (
            foo : in std_logic;
            bar : in std_logic;
            zoo : out std_logic
        );
    end entity;

    architecture rtl of temp is

        -- conversion functions
        function hc_uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;
        function hc_uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;
        function hc_sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;
        function hc_sgn(a : std_logic_vector) return signed           is begin return signed(a); end;
        function hc_sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;
        function hc_sl (a : unsigned)         return std_logic        is begin return a(a'right); end;
        function hc_sl (a : signed)           return std_logic        is begin return a(a'right); end;
        function hc_sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;
        function hc_slv(a : std_logic_vector) return std_logic_vector is begin return a; end;
        function hc_slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;
        function hc_slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;
        signal hc_2 : std_logic;

    begin

        hc_2 <= foo;
        zoo <= hc_2;

    end architecture;
    |}]
;;
