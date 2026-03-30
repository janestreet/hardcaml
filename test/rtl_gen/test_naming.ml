open! Core
open Hardcaml
open Expect_test_helpers_core

module T_simple = struct
  type 'a t = { simple : 'a } [@@deriving hardcaml]
end

module T_extended = struct
  type 'a t =
    { simple : 'a
    ; extended_in_both : 'a [@rtlname "extended_in_?both"]
    ; extended_in_vhdl_only : 'a [@rtlname "extended_in_$vhdl_only"]
    }
  [@@deriving hardcaml]
end

module Make_circuit (T : Interface.S) = struct
  module I =
    Interface.Update
      (T)
      (struct
        let port_names_and_widths =
          T.map2 T.port_names T.port_widths ~f:(fun name width -> "i_" ^ name, width)
        ;;
      end)

  module O =
    Interface.Update
      (T)
      (struct
        let port_names_and_widths =
          T.map2 T.port_names T.port_widths ~f:(fun name width -> "o_" ^ name, width)
        ;;
      end)

  module H = Hierarchy.In_scope (I) (O)

  let create_inner _scope = Fn.id
  let create_outer scope = H.hierarchical ~scope create_inner ~name:"inner"
  let hierarchical scope = H.hierarchical ~scope create_outer

  let circuit =
    let module C = Circuit.With_interface (I) (O) in
    let scope = Scope.create () in
    scope, C.create_exn ~name:"naming" (hierarchical scope)
  ;;
end

module Circuit_simple = Make_circuit (T_simple)
module Circuit_extended = Make_circuit (T_extended)

let print lang (scope, circuit) =
  Rtl.print ~database:(Scope.circuit_database scope) lang circuit
;;

let%expect_test "verilog" =
  print Verilog Circuit_simple.circuit;
  [%expect
    {|
    module inner (
        i_simple,
        o_simple
    );

        input i_simple;
        output o_simple;

        wire _2;
        assign _2 = i_simple;
        assign o_simple = _2;

    endmodule
    module test_naming (
        i_simple,
        o_simple
    );

        input i_simple;
        output o_simple;

        wire _2;
        wire _5;
        wire _3;
        assign _2 = i_simple;
        inner
            inner
            ( .i_simple(_2),
              .o_simple(_5) );
        assign _3 = _5;
        assign o_simple = _3;

    endmodule
    module naming (
        i_simple,
        o_simple
    );

        input i_simple;
        output o_simple;

        wire _2;
        wire _5;
        wire _3;
        assign _2 = i_simple;
        test_naming
            test_naming
            ( .i_simple(_2),
              .o_simple(_5) );
        assign _3 = _5;
        assign o_simple = _3;

    endmodule
    |}];
  print Verilog Circuit_extended.circuit;
  [%expect
    {|
    module inner (
        i_extended_in_$vhdl_only,
        \i_extended_in_?both ,
        i_simple,
        o_simple,
        \o_extended_in_?both ,
        o_extended_in_$vhdl_only
    );

        input i_extended_in_$vhdl_only;
        input \i_extended_in_?both ;
        input i_simple;
        output o_simple;
        output \o_extended_in_?both ;
        output o_extended_in_$vhdl_only;

        wire _2;
        wire _5;
        wire _8;
        assign _2 = i_extended_in_$vhdl_only;
        assign _5 = \i_extended_in_?both ;
        assign _8 = i_simple;
        assign o_simple = _8;
        assign \o_extended_in_?both  = _5;
        assign o_extended_in_$vhdl_only = _2;

    endmodule
    module test_naming (
        i_extended_in_$vhdl_only,
        \i_extended_in_?both ,
        i_simple,
        o_simple,
        \o_extended_in_?both ,
        o_extended_in_$vhdl_only
    );

        input i_extended_in_$vhdl_only;
        input \i_extended_in_?both ;
        input i_simple;
        output o_simple;
        output \o_extended_in_?both ;
        output o_extended_in_$vhdl_only;

        wire _11;
        wire _12;
        wire _4;
        wire _6;
        wire _8;
        wire [2:0] _10;
        wire _13;
        assign _11 = _10[2:2];
        assign _12 = _10[1:1];
        assign _4 = i_extended_in_$vhdl_only;
        assign _6 = \i_extended_in_?both ;
        assign _8 = i_simple;
        inner
            inner
            ( .i_simple(_8),
              .\i_extended_in_?both (_6),
              .i_extended_in_$vhdl_only(_4),
              .o_simple(_10[0:0]),
              .\o_extended_in_?both (_10[1:1]),
              .o_extended_in_$vhdl_only(_10[2:2]) );
        assign _13 = _10[0:0];
        assign o_simple = _13;
        assign \o_extended_in_?both  = _12;
        assign o_extended_in_$vhdl_only = _11;

    endmodule
    module naming (
        i_extended_in_$vhdl_only,
        \i_extended_in_?both ,
        i_simple,
        o_simple,
        \o_extended_in_?both ,
        o_extended_in_$vhdl_only
    );

        input i_extended_in_$vhdl_only;
        input \i_extended_in_?both ;
        input i_simple;
        output o_simple;
        output \o_extended_in_?both ;
        output o_extended_in_$vhdl_only;

        wire _11;
        wire _12;
        wire _4;
        wire _6;
        wire _8;
        wire [2:0] _10;
        wire _13;
        assign _11 = _10[2:2];
        assign _12 = _10[1:1];
        assign _4 = i_extended_in_$vhdl_only;
        assign _6 = \i_extended_in_?both ;
        assign _8 = i_simple;
        test_naming
            test_naming
            ( .i_simple(_8),
              .\i_extended_in_?both (_6),
              .i_extended_in_$vhdl_only(_4),
              .o_simple(_10[0:0]),
              .\o_extended_in_?both (_10[1:1]),
              .o_extended_in_$vhdl_only(_10[2:2]) );
        assign _13 = _10[0:0];
        assign o_simple = _13;
        assign \o_extended_in_?both  = _12;
        assign o_extended_in_$vhdl_only = _11;

    endmodule
    |}]
;;

let%expect_test "vhdl" =
  print Vhdl Circuit_simple.circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity inner is
        port (
            i_simple : in std_logic;
            o_simple : out std_logic
        );
    end entity;

    architecture rtl of inner is

        signal \_2\ : std_logic;

    begin

        \_2\ <= i_simple;
        o_simple <= \_2\;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity test_naming is
        port (
            i_simple : in std_logic;
            o_simple : out std_logic
        );
    end entity;

    architecture rtl of test_naming is

        signal \_2\ : std_logic;
        signal \_5\ : std_logic;
        signal \_3\ : std_logic;

    begin

        \_2\ <= i_simple;
        inner: entity work.inner (rtl)
            port map ( i_simple => \_2\,
                       o_simple => \_5\ );
        \_3\ <= \_5\;
        o_simple <= \_3\;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity naming is
        port (
            i_simple : in std_logic;
            o_simple : out std_logic
        );
    end entity;

    architecture rtl of naming is

        signal \_2\ : std_logic;
        signal \_5\ : std_logic;
        signal \_3\ : std_logic;

    begin

        \_2\ <= i_simple;
        test_naming: entity work.test_naming (rtl)
            port map ( i_simple => \_2\,
                       o_simple => \_5\ );
        \_3\ <= \_5\;
        o_simple <= \_3\;

    end architecture;
    |}];
  print Vhdl Circuit_extended.circuit;
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity inner is
        port (
            \i_extended_in_$vhdl_only\ : in std_logic;
            \i_extended_in_?both\ : in std_logic;
            i_simple : in std_logic;
            o_simple : out std_logic;
            \o_extended_in_?both\ : out std_logic;
            \o_extended_in_$vhdl_only\ : out std_logic
        );
    end entity;

    architecture rtl of inner is

        signal \_2\ : std_logic;
        signal \_5\ : std_logic;
        signal \_8\ : std_logic;

    begin

        \_2\ <= \i_extended_in_$vhdl_only\;
        \_5\ <= \i_extended_in_?both\;
        \_8\ <= i_simple;
        o_simple <= \_8\;
        \o_extended_in_?both\ <= \_5\;
        \o_extended_in_$vhdl_only\ <= \_2\;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity test_naming is
        port (
            \i_extended_in_$vhdl_only\ : in std_logic;
            \i_extended_in_?both\ : in std_logic;
            i_simple : in std_logic;
            o_simple : out std_logic;
            \o_extended_in_?both\ : out std_logic;
            \o_extended_in_$vhdl_only\ : out std_logic
        );
    end entity;

    architecture rtl of test_naming is

        signal \_11\ : std_logic;
        signal \_12\ : std_logic;
        signal \_4\ : std_logic;
        signal \_6\ : std_logic;
        signal \_8\ : std_logic;
        signal \_10\ : std_logic_vector(2 downto 0);
        signal \_13\ : std_logic;

    begin

        \_11\ <= \_10\(2);
        \_12\ <= \_10\(1);
        \_4\ <= \i_extended_in_$vhdl_only\;
        \_6\ <= \i_extended_in_?both\;
        \_8\ <= i_simple;
        inner: entity work.inner (rtl)
            port map ( i_simple => \_8\,
                       \i_extended_in_?both\ => \_6\,
                       \i_extended_in_$vhdl_only\ => \_4\,
                       o_simple => \_10\(0),
                       \o_extended_in_?both\ => \_10\(1),
                       \o_extended_in_$vhdl_only\ => \_10\(2) );
        \_13\ <= \_10\(0);
        o_simple <= \_13\;
        \o_extended_in_?both\ <= \_12\;
        \o_extended_in_$vhdl_only\ <= \_11\;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity naming is
        port (
            \i_extended_in_$vhdl_only\ : in std_logic;
            \i_extended_in_?both\ : in std_logic;
            i_simple : in std_logic;
            o_simple : out std_logic;
            \o_extended_in_?both\ : out std_logic;
            \o_extended_in_$vhdl_only\ : out std_logic
        );
    end entity;

    architecture rtl of naming is

        signal \_11\ : std_logic;
        signal \_12\ : std_logic;
        signal \_4\ : std_logic;
        signal \_6\ : std_logic;
        signal \_8\ : std_logic;
        signal \_10\ : std_logic_vector(2 downto 0);
        signal \_13\ : std_logic;

    begin

        \_11\ <= \_10\(2);
        \_12\ <= \_10\(1);
        \_4\ <= \i_extended_in_$vhdl_only\;
        \_6\ <= \i_extended_in_?both\;
        \_8\ <= i_simple;
        test_naming: entity work.test_naming (rtl)
            port map ( i_simple => \_8\,
                       \i_extended_in_?both\ => \_6\,
                       \i_extended_in_$vhdl_only\ => \_4\,
                       o_simple => \_10\(0),
                       \o_extended_in_?both\ => \_10\(1),
                       \o_extended_in_$vhdl_only\ => \_10\(2) );
        \_13\ <= \_10\(0);
        o_simple <= \_13\;
        \o_extended_in_?both\ <= \_12\;
        \o_extended_in_$vhdl_only\ <= \_11\;

    end architecture;
    |}]
;;

(* Extended identifiers in verilog dont allow spaces, so we should ban them. *)
let%expect_test "Cant allow spaces or backslashes" =
  let open Signal in
  require_does_raise (fun () ->
    let circuit =
      Circuit.create_exn ~name:"invalid" [ output "dont allow spaces" (input "x" 1) ]
    in
    let scope = Scope.create () in
    print Verilog (scope, circuit));
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name invalid)
      (hierarchy_path (invalid))
      (exn (
        "[Rtl_name]s must only contain printable characters and may not contain spaces or back slashes"
        (identifier "dont allow spaces"))))
    |}];
  require_does_raise (fun () ->
    let circuit =
      Circuit.create_exn
        ~name:"invalid"
        [ output {|dont\allow\backslashes|} (input "x" 1) ]
    in
    let scope = Scope.create () in
    print Verilog (scope, circuit));
  [%expect
    {|
    ("Error while writing circuit"
      (circuit_name invalid)
      (hierarchy_path (invalid))
      (exn (
        "[Rtl_name]s must only contain printable characters and may not contain spaces or back slashes"
        (identifier "dont\\allow\\backslashes"))))
    |}]
;;
