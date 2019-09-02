open! Import
open! Parameter

let show t = print_s [%sexp (t : t)]
let is_subset ts1 ts2 = print_s [%sexp (is_subset ts1 ts2 : bool)]
let sort_by_name ts = print_s [%sexp (sort_by_name ts : t list)]

let find_name ts name =
  print_s [%sexp (find_name ts (Parameter_name.of_string name) : Value.t Option.t)]
;;

let find_name_exn ts name =
  print_s
    [%sexp
      (try_with (fun () -> find_name_exn ts (Parameter_name.of_string name))
       : Value.t Or_error.t)]
;;

let t1 = { name = Parameter_name.of_string "N1"; value = Int 1 }
let t2 = { name = Parameter_name.of_string "N2"; value = Int 2 }

let%expect_test "[sexp_of_t]" =
  show t1;
  [%expect {|
    (N1 1) |}]
;;

let%expect_test "[find_name]" =
  find_name [] "foo";
  [%expect {|
    () |}];
  find_name [ t1 ] "N1";
  [%expect {|
    (1) |}];
  find_name [ t1 ] "N2";
  [%expect {|
    () |}];
  find_name [ t1; t2 ] "N2";
  [%expect {|
    (2) |}]
;;

let%expect_test "[find_name_exn]" =
  find_name_exn [] "foo";
  [%expect {|
    (Error ("couldn't find parameter" (name foo) (parameters ()))) |}];
  find_name_exn [ t1 ] "N1";
  [%expect {|
    (Ok 1) |}];
  find_name_exn [ t1 ] "N2";
  [%expect {|
    (Error ("couldn't find parameter" (name N2) (parameters ((N1 1))))) |}];
  find_name_exn [ t1; t2 ] "N2";
  [%expect {|
    (Ok 2) |}]
;;

let%expect_test "[is_subset]" =
  is_subset [] [];
  [%expect {|
    true |}];
  is_subset [] [ t1 ];
  [%expect {|
    true |}];
  is_subset [ t1 ] [];
  [%expect {|
    false |}];
  is_subset [ t1 ] [ t1 ];
  [%expect {|
    true |}];
  is_subset [ t1 ] [ t2 ];
  [%expect {|
    false |}]
;;

let%expect_test "[sort_by_name]" =
  sort_by_name [];
  [%expect {|
    () |}];
  sort_by_name [ t1 ];
  [%expect {|
    ((N1 1)) |}];
  sort_by_name [ t1; t2 ];
  [%expect {|
    ((N1 1)
     (N2 2)) |}];
  sort_by_name [ t2; t1 ];
  [%expect {|
    ((N1 1)
     (N2 2)) |}]
;;

let%expect_test "std_logic rountrip" =
  let roundtrip std_logic =
    let char = Std_logic.to_char std_logic in
    let converted = try_with (fun () -> Std_logic.of_char_exn char) in
    print_s
      [%message
        "" (std_logic : Std_logic.t) (char : char) (converted : Std_logic.t Or_error.t)];
    require
      [%here]
      (match converted with
       | Error _ -> false
       | Ok x -> Std_logic.equal x std_logic)
  in
  List.iter Std_logic.all ~f:roundtrip;
  [%expect
    {|
    ((std_logic U)
     (char      U)
     (converted (Ok U)))
    ((std_logic X)
     (char      X)
     (converted (Ok X)))
    ((std_logic 0)
     (char      0)
     (converted (Ok 0)))
    ((std_logic 1)
     (char      1)
     (converted (Ok 1)))
    ((std_logic Z)
     (char      Z)
     (converted (Ok Z)))
    ((std_logic W)
     (char      W)
     (converted (Ok W)))
    ((std_logic L)
     (char      L)
     (converted (Ok L)))
    ((std_logic H)
     (char      H)
     (converted (Ok H)))
    ((std_logic _)
     (char      _)
     (converted (Ok _))) |}]
;;

let%expect_test "bad std_logic" =
  show_raise (fun () -> Std_logic.of_char_exn 'a');
  [%expect {| (raised ("[Std_logic.of_char_exn] got invalid char" (char a))) |}]
;;

let%expect_test "std_logic_vector" =
  print_s [%message "" ~_:(Std_logic_vector.create Std_logic.all : Std_logic_vector.t)];
  [%expect {| UX01ZWLH_ |}]
;;

let%expect_test "bad std_logic_vector" =
  show_raise (fun () -> Std_logic_vector.of_string "1Ab0");
  [%expect {| (raised ("[Std_logic.of_char_exn] got invalid char" (char A))) |}]
;;

let%expect_test "bit_vector" =
  print_s [%message "" ~_:(Bit_vector.create [ true; false; true ] : Bit_vector.t)];
  [%expect {| 101 |}]
;;

let%expect_test "bad bit_vector" =
  show_raise (fun () -> Bit_vector.of_string "1U0");
  [%expect {| (raised ("[Bit_vector.of_string] got invalid char" (char U))) |}]
;;

let create_instantiation_test =
  let parameters =
    List.map
      ~f:(fun (name, value) -> create ~name ~value)
      [ "an_int", Value.Int 7
      ; "a_bool", Bool true
      ; "a_string", String "world"
      ; "a_real", Real 3.9
      ; "a_bit", Bit true
      ; "a_bit_vector", Bit_vector (Bit_vector.of_string "1100")
      ; "a_std_logic", Std_logic Std_logic.W
      ; "a_std_ulogic", Std_ulogic Std_logic.L1
      ; "a_std_logic_vector", Std_logic_vector (Std_logic_vector.of_string "1010")
      ; "a_std_ulogic_vector", Std_ulogic_vector (Std_logic_vector.of_string "1011")
      ]
  in
  let inst hdl a =
    (Instantiation.create
       ()
       ~name:("test_parameters_" ^ hdl)
       ~parameters
       ~inputs:[ "a", a ]
       ~outputs:[ "b", 2 ])
    #o
      "b"
    |> Signal.output ("b_" ^ hdl)
  in
  let a = Signal.input "a" 1 in
  let circ hdl =
    Circuit.create_exn
      ~name:("test_parameter_instantiation_" ^ hdl)
      [ inst "vhdl" a; inst "verilog" a ]
  in
  circ
;;

let%expect_test "instantiation in verilog" =
  Rtl.print Verilog (create_instantiation_test "verilog");
  [%expect
    {|
    module test_parameter_instantiation_verilog (
        a,
        b_vhdl,
        b_verilog
    );

        input a;
        output [1:0] b_vhdl;
        output [1:0] b_verilog;

        /* signal declarations */
        wire [1:0] _5;
        wire [1:0] _7;

        /* logic */
        test_parameters_verilog
            #( .an_int(7), .a_bool(1'b1), .a_string("world"), .a_real(3.900000), .a_bit(1'b1), .a_bit_vector(4'b1100), .a_std_logic(4'd5), .a_std_ulogic(4'd3), .a_std_logic_vector(4'b1010), .a_std_ulogic_vector(4'b1011) )
            the_test_parameters_verilog
            ( .a(a), .b(_5[1:0]) );
        test_parameters_vhdl
            #( .an_int(7), .a_bool(1'b1), .a_string("world"), .a_real(3.900000), .a_bit(1'b1), .a_bit_vector(4'b1100), .a_std_logic(4'd5), .a_std_ulogic(4'd3), .a_std_logic_vector(4'b1010), .a_std_ulogic_vector(4'b1011) )
            the_test_parameters_vhdl
            ( .a(a), .b(_7[1:0]) );

        /* aliases */

        /* output assignments */
        assign b_vhdl = _7;
        assign b_verilog = _5;

    endmodule |}]
;;

let%expect_test "instantiation in vhdl" =
  Rtl.print Vhdl (create_instantiation_test "vhdl");
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity test_parameter_instantiation_vhdl is
        port (
            a : in std_logic;
            b_vhdl : out std_logic_vector (1 downto 0);
            b_verilog : out std_logic_vector (1 downto 0)
        );
    end entity;

    architecture rtl of test_parameter_instantiation_vhdl is

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

        -- signal declarations
        signal hc_5 : std_logic_vector (1 downto 0);
        signal hc_7 : std_logic_vector (1 downto 0);

    begin

        -- logic
        the_test_parameters_verilog: entity work.test_parameters_verilog (rtl)
            generic map ( an_int => 7, a_bool => true, a_string => "world", a_real => 3.900000, a_bit => '1', a_bit_vector => "1100", a_std_logic => 'W', a_std_ulogic => '1', a_std_logic_vector => std_logic_vector'("1010"), a_std_ulogic_vector => std_ulogic_vector'("1011"))
            port map ( a => a, b => hc_5(1 downto 0) );
        the_test_parameters_vhdl: entity work.test_parameters_vhdl (rtl)
            generic map ( an_int => 7, a_bool => true, a_string => "world", a_real => 3.900000, a_bit => '1', a_bit_vector => "1100", a_std_logic => 'W', a_std_ulogic => '1', a_std_logic_vector => std_logic_vector'("1010"), a_std_ulogic_vector => std_ulogic_vector'("1011"))
            port map ( a => a, b => hc_7(1 downto 0) );

        -- aliases

        -- output assignments
        b_vhdl <= hc_7;
        b_verilog <= hc_5;

    end architecture; |}]
;;
