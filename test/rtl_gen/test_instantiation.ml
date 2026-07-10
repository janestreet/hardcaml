open! Core
open Hardcaml
open Expect_test_helpers_core

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
        foo,
        bar,
        zoo,
        moo
    );

        input foo;
        input bar;
        output zoo;
        output [1:0] moo;

        wire [1:0] signal_select;
        wire [1:0] signal_select_1;
        wire [1:0] signal_select_2;
        wire [1:0] signal_or;
        wire [1:0] signal_or_1;
        wire [2:0] signal_inst;
        wire signal_select_3;
        wire [2:0] signal_inst_1;
        wire signal_select_4;
        wire signal_wire;
        wire signal_wire_1;
        wire [2:0] signal_inst_2;
        wire signal_select_5;
        wire signal_or_2;
        wire signal_or_3;
        assign signal_select = signal_inst[2:1];
        assign signal_select_1 = signal_inst_1[2:1];
        assign signal_select_2 = signal_inst_2[2:1];
        assign signal_or = signal_select_2 | signal_select_1;
        assign signal_or_1 = signal_or | signal_select;
        foo
            #( .par(3),
               .far("baloo") )
            the_foo
            ( .foo(signal_wire_1),
              .bar(signal_wire),
              .zoo(signal_inst[0:0]),
              .moo(signal_inst[2:1]) );
        assign signal_select_3 = signal_inst[0:0];
        foo
            #( .par(3) )
            the_foo_1
            ( .foo(signal_wire_1),
              .bar(signal_wire),
              .zoo(signal_inst_1[0:0]),
              .moo(signal_inst_1[2:1]) );
        assign signal_select_4 = signal_inst_1[0:0];
        assign signal_wire = bar;
        assign signal_wire_1 = foo;
        foo
            the_foo_2
            ( .foo(signal_wire_1),
              .bar(signal_wire),
              .zoo(signal_inst_2[0:0]),
              .moo(signal_inst_2[2:1]) );
        assign signal_select_5 = signal_inst_2[0:0];
        assign signal_or_2 = signal_select_5 | signal_select_4;
        assign signal_or_3 = signal_or_2 | signal_select_3;
        assign zoo = signal_or_3;
        assign moo = signal_or_1;

    endmodule
    ("GHDL failed with" (error_code (Error (Exit_non_zero 1))))
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity temp is
        port (
            foo : in std_logic;
            bar : in std_logic;
            zoo : out std_logic;
            moo : out std_logic_vector(1 downto 0)
        );
    end entity;

    architecture rtl of temp is

        signal signal_select : std_logic_vector(1 downto 0);
        signal signal_select_1 : std_logic_vector(1 downto 0);
        signal signal_select_2 : std_logic_vector(1 downto 0);
        signal signal_or : std_logic_vector(1 downto 0);
        signal signal_or_1 : std_logic_vector(1 downto 0);
        signal signal_inst : std_logic_vector(2 downto 0);
        signal signal_select_3 : std_logic;
        signal signal_inst_1 : std_logic_vector(2 downto 0);
        signal signal_select_4 : std_logic;
        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_inst_2 : std_logic_vector(2 downto 0);
        signal signal_select_5 : std_logic;
        signal signal_or_2 : std_logic;
        signal signal_or_3 : std_logic;

    begin

        signal_select <= signal_inst(2 downto 1);
        signal_select_1 <= signal_inst_1(2 downto 1);
        signal_select_2 <= signal_inst_2(2 downto 1);
        signal_or <= signal_select_2 or signal_select_1;
        signal_or_1 <= signal_or or signal_select;
        the_foo: entity work.foo (rtl)
            generic map ( par => 3,
                          far => "baloo" )
            port map ( foo => signal_wire_1,
                       bar => signal_wire,
                       zoo => signal_inst(0),
                       moo => signal_inst(2 downto 1) );
        signal_select_3 <= signal_inst(0);
        the_foo_1: entity work.foo (rtl)
            generic map ( par => 3 )
            port map ( foo => signal_wire_1,
                       bar => signal_wire,
                       zoo => signal_inst_1(0),
                       moo => signal_inst_1(2 downto 1) );
        signal_select_4 <= signal_inst_1(0);
        signal_wire <= bar;
        signal_wire_1 <= foo;
        the_foo_2: entity work.foo (rtl)
            port map ( foo => signal_wire_1,
                       bar => signal_wire,
                       zoo => signal_inst_2(0),
                       moo => signal_inst_2(2 downto 1) );
        signal_select_5 <= signal_inst_2(0);
        signal_or_2 <= signal_select_5 or signal_select_4;
        signal_or_3 <= signal_or_2 or signal_select_3;
        zoo <= signal_or_3;
        moo <= signal_or_1;

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
        foo,
        bar,
        zoo
    );

        input foo;
        input bar;
        output zoo;

        wire signal_wire;
        wire signal_wire_1;
        wire signal_inst;
        wire signal_wire_2;
        assign signal_wire = bar;
        assign signal_wire_1 = foo;
        foo
            the_foo
            ( .foo(signal_wire_1),
              .bar(signal_wire),
              .zoo(signal_inst) );
        assign signal_wire_2 = signal_inst;
        assign zoo = signal_wire_2;

    endmodule
    ("GHDL failed with" (error_code (Error (Exit_non_zero 1))))
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

        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_inst : std_logic;
        signal signal_wire_2 : std_logic;

    begin

        signal_wire <= bar;
        signal_wire_1 <= foo;
        the_foo: entity work.foo (rtl)
            port map ( foo => signal_wire_1,
                       bar => signal_wire,
                       zoo => signal_inst );
        signal_wire_2 <= signal_inst;
        zoo <= signal_wire_2;

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
  let config = { Rtl.Config.default with backend = Modelsim } in
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
  Testing.analyse_vhdl_and_verilog ~quiet:true ~show:true ~config circuit;
  [%expect
    {|
    ("Icarus Verilog failed with" (error_code (Error (Exit_non_zero 3))))
    module temp (
        foo,
        bar,
        zoo
    );

        input foo;
        input bar;
        output zoo;

        wire signal_wire;
        wire signal_wire_1;
        wire signal_inst;
        wire signal_wire_2;
        assign signal_wire = bar;
        assign signal_wire_1 = foo;
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
            ( .foo(signal_wire_1),
              .bar(signal_wire),
              .zoo(signal_inst) );
        assign signal_wire_2 = signal_inst;
        assign zoo = signal_wire_2;

    endmodule
    ("GHDL failed with" (error_code (Error (Exit_non_zero 1))))
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

        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_inst : std_logic;
        signal signal_wire_2 : std_logic;

    begin

        signal_wire <= bar;
        signal_wire_1 <= foo;
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
            port map ( foo => signal_wire_1,
                       bar => signal_wire,
                       zoo => signal_inst );
        signal_wire_2 <= signal_inst;
        zoo <= signal_wire_2;

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

        wire signal_wire;
        assign signal_wire = foo;
        assign zoo = signal_wire;

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

        signal signal_wire : std_logic;

    begin

        signal_wire <= foo;
        zoo <= signal_wire;

    end architecture;
    |}]
;;

(* Ensure (resolved) Std_[{u}]logic values are mapped to bit types in Verilog for the
   (default) Vivado compatibility mode. Tests above show the mapping for Modelsim. *)
let%expect_test "vivado compatibility mode" =
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
  let circuit ~with_dodgy_std_logic_parameter =
    C.create_exn ~name:"temp" (fun (i : _ I.t) ->
      let module I = Instantiation.With_interface (I) (O) in
      I.create
        ~name:"foo"
        ~parameters:
          ([ Parameter.create ~name:"a" ~value:(Std_logic L0)
           ; Parameter.create ~name:"b" ~value:(Std_logic L1)
           ; Parameter.create ~name:"c" ~value:(Std_logic L0)
           ; Parameter.create ~name:"d" ~value:(Std_logic L1)
           ]
           @
           if with_dodgy_std_logic_parameter
           then [ Parameter.create ~name:"e" ~value:(Std_logic U) ]
           else [])
        i)
  in
  let config = { Rtl.Config.default with backend = Vivado } in
  Testing.analyse_vhdl_and_verilog
    ~quiet:true
    ~show:true
    ~config
    (circuit ~with_dodgy_std_logic_parameter:false);
  [%expect
    {|
    ("Icarus Verilog failed with" (error_code (Error (Exit_non_zero 2))))
    module temp (
        foo,
        bar,
        zoo
    );

        input foo;
        input bar;
        output zoo;

        wire signal_wire;
        wire signal_wire_1;
        wire signal_inst;
        wire signal_wire_2;
        assign signal_wire = bar;
        assign signal_wire_1 = foo;
        foo
            #( .a(1'b0),
               .b(1'b1),
               .c(1'b0),
               .d(1'b1) )
            the_foo
            ( .foo(signal_wire_1),
              .bar(signal_wire),
              .zoo(signal_inst) );
        assign signal_wire_2 = signal_inst;
        assign zoo = signal_wire_2;

    endmodule
    ("GHDL failed with" (error_code (Error (Exit_non_zero 1))))
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

        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_inst : std_logic;
        signal signal_wire_2 : std_logic;

    begin

        signal_wire <= bar;
        signal_wire_1 <= foo;
        the_foo: entity work.foo (rtl)
            generic map ( a => '0',
                          b => '1',
                          c => '0',
                          d => '1' )
            port map ( foo => signal_wire_1,
                       bar => signal_wire,
                       zoo => signal_inst );
        signal_wire_2 <= signal_inst;
        zoo <= signal_wire_2;

    end architecture;
    |}];
  require_does_raise (fun () ->
    Testing.analyse_vhdl_and_verilog
      ~quiet:true
      ~show:true
      ~config
      (circuit ~with_dodgy_std_logic_parameter:true));
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name temp)
      (hierarchy_path (temp))
      (exn (
        "[Rtl_ast] failed to create statement for signal"
        (signal (
          instantiation
          (width 1)
          ("work.foo(rtl){the_foo}"
            (parameters (
              (a (Std_logic 0))
              (b (Std_logic 1))
              (c (Std_logic 0))
              (d (Std_logic 1))
              (e (Std_logic U))))
            (inputs (
              (foo wire)
              (bar wire)))
            (outputs ((zoo 1))))))
        (exn ("Cannot map Std_logic value to Bit type" (v U))))))
    |}]
;;
