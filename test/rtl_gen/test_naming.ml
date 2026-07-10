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

        wire signal_wire;
        assign signal_wire = i_simple;
        assign o_simple = signal_wire;

    endmodule
    module test_naming (
        i_simple,
        o_simple
    );

        input i_simple;
        output o_simple;

        wire signal_wire;
        wire signal_inst;
        wire signal_wire_1;
        assign signal_wire = i_simple;
        inner
            inner
            ( .i_simple(signal_wire),
              .o_simple(signal_inst) );
        assign signal_wire_1 = signal_inst;
        assign o_simple = signal_wire_1;

    endmodule
    module naming (
        i_simple,
        o_simple
    );

        input i_simple;
        output o_simple;

        wire signal_wire;
        wire signal_inst;
        wire signal_wire_1;
        assign signal_wire = i_simple;
        test_naming
            test_naming
            ( .i_simple(signal_wire),
              .o_simple(signal_inst) );
        assign signal_wire_1 = signal_inst;
        assign o_simple = signal_wire_1;

    endmodule
    |}];
  print Verilog Circuit_extended.circuit;
  [%expect
    {|
    module inner (
        i_simple,
        \i_extended_in_?both ,
        i_extended_in_$vhdl_only,
        o_simple,
        \o_extended_in_?both ,
        o_extended_in_$vhdl_only
    );

        input i_simple;
        input \i_extended_in_?both ;
        input i_extended_in_$vhdl_only;
        output o_simple;
        output \o_extended_in_?both ;
        output o_extended_in_$vhdl_only;

        wire signal_wire;
        wire signal_wire_1;
        wire signal_wire_2;
        assign signal_wire = i_extended_in_$vhdl_only;
        assign signal_wire_1 = \i_extended_in_?both ;
        assign signal_wire_2 = i_simple;
        assign o_simple = signal_wire_2;
        assign \o_extended_in_?both  = signal_wire_1;
        assign o_extended_in_$vhdl_only = signal_wire;

    endmodule
    module test_naming (
        i_simple,
        \i_extended_in_?both ,
        i_extended_in_$vhdl_only,
        o_simple,
        \o_extended_in_?both ,
        o_extended_in_$vhdl_only
    );

        input i_simple;
        input \i_extended_in_?both ;
        input i_extended_in_$vhdl_only;
        output o_simple;
        output \o_extended_in_?both ;
        output o_extended_in_$vhdl_only;

        wire signal_select;
        wire signal_select_1;
        wire signal_wire;
        wire signal_wire_1;
        wire signal_wire_2;
        wire [2:0] signal_inst;
        wire signal_select_2;
        assign signal_select = signal_inst[2:2];
        assign signal_select_1 = signal_inst[1:1];
        assign signal_wire = i_extended_in_$vhdl_only;
        assign signal_wire_1 = \i_extended_in_?both ;
        assign signal_wire_2 = i_simple;
        inner
            inner
            ( .i_simple(signal_wire_2),
              .\i_extended_in_?both (signal_wire_1),
              .i_extended_in_$vhdl_only(signal_wire),
              .o_simple(signal_inst[0:0]),
              .\o_extended_in_?both (signal_inst[1:1]),
              .o_extended_in_$vhdl_only(signal_inst[2:2]) );
        assign signal_select_2 = signal_inst[0:0];
        assign o_simple = signal_select_2;
        assign \o_extended_in_?both  = signal_select_1;
        assign o_extended_in_$vhdl_only = signal_select;

    endmodule
    module naming (
        i_simple,
        \i_extended_in_?both ,
        i_extended_in_$vhdl_only,
        o_simple,
        \o_extended_in_?both ,
        o_extended_in_$vhdl_only
    );

        input i_simple;
        input \i_extended_in_?both ;
        input i_extended_in_$vhdl_only;
        output o_simple;
        output \o_extended_in_?both ;
        output o_extended_in_$vhdl_only;

        wire signal_select;
        wire signal_select_1;
        wire signal_wire;
        wire signal_wire_1;
        wire signal_wire_2;
        wire [2:0] signal_inst;
        wire signal_select_2;
        assign signal_select = signal_inst[2:2];
        assign signal_select_1 = signal_inst[1:1];
        assign signal_wire = i_extended_in_$vhdl_only;
        assign signal_wire_1 = \i_extended_in_?both ;
        assign signal_wire_2 = i_simple;
        test_naming
            test_naming
            ( .i_simple(signal_wire_2),
              .\i_extended_in_?both (signal_wire_1),
              .i_extended_in_$vhdl_only(signal_wire),
              .o_simple(signal_inst[0:0]),
              .\o_extended_in_?both (signal_inst[1:1]),
              .o_extended_in_$vhdl_only(signal_inst[2:2]) );
        assign signal_select_2 = signal_inst[0:0];
        assign o_simple = signal_select_2;
        assign \o_extended_in_?both  = signal_select_1;
        assign o_extended_in_$vhdl_only = signal_select;

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

        signal signal_wire : std_logic;

    begin

        signal_wire <= i_simple;
        o_simple <= signal_wire;

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

        signal signal_wire : std_logic;
        signal signal_inst : std_logic;
        signal signal_wire_1 : std_logic;

    begin

        signal_wire <= i_simple;
        inner: entity work.inner (rtl)
            port map ( i_simple => signal_wire,
                       o_simple => signal_inst );
        signal_wire_1 <= signal_inst;
        o_simple <= signal_wire_1;

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

        signal signal_wire : std_logic;
        signal signal_inst : std_logic;
        signal signal_wire_1 : std_logic;

    begin

        signal_wire <= i_simple;
        test_naming: entity work.test_naming (rtl)
            port map ( i_simple => signal_wire,
                       o_simple => signal_inst );
        signal_wire_1 <= signal_inst;
        o_simple <= signal_wire_1;

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
            i_simple : in std_logic;
            \i_extended_in_?both\ : in std_logic;
            \i_extended_in_$vhdl_only\ : in std_logic;
            o_simple : out std_logic;
            \o_extended_in_?both\ : out std_logic;
            \o_extended_in_$vhdl_only\ : out std_logic
        );
    end entity;

    architecture rtl of inner is

        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_wire_2 : std_logic;

    begin

        signal_wire <= \i_extended_in_$vhdl_only\;
        signal_wire_1 <= \i_extended_in_?both\;
        signal_wire_2 <= i_simple;
        o_simple <= signal_wire_2;
        \o_extended_in_?both\ <= signal_wire_1;
        \o_extended_in_$vhdl_only\ <= signal_wire;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity test_naming is
        port (
            i_simple : in std_logic;
            \i_extended_in_?both\ : in std_logic;
            \i_extended_in_$vhdl_only\ : in std_logic;
            o_simple : out std_logic;
            \o_extended_in_?both\ : out std_logic;
            \o_extended_in_$vhdl_only\ : out std_logic
        );
    end entity;

    architecture rtl of test_naming is

        signal signal_select : std_logic;
        signal signal_select_1 : std_logic;
        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_wire_2 : std_logic;
        signal signal_inst : std_logic_vector(2 downto 0);
        signal signal_select_2 : std_logic;

    begin

        signal_select <= signal_inst(2);
        signal_select_1 <= signal_inst(1);
        signal_wire <= \i_extended_in_$vhdl_only\;
        signal_wire_1 <= \i_extended_in_?both\;
        signal_wire_2 <= i_simple;
        inner: entity work.inner (rtl)
            port map ( i_simple => signal_wire_2,
                       \i_extended_in_?both\ => signal_wire_1,
                       \i_extended_in_$vhdl_only\ => signal_wire,
                       o_simple => signal_inst(0),
                       \o_extended_in_?both\ => signal_inst(1),
                       \o_extended_in_$vhdl_only\ => signal_inst(2) );
        signal_select_2 <= signal_inst(0);
        o_simple <= signal_select_2;
        \o_extended_in_?both\ <= signal_select_1;
        \o_extended_in_$vhdl_only\ <= signal_select;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity naming is
        port (
            i_simple : in std_logic;
            \i_extended_in_?both\ : in std_logic;
            \i_extended_in_$vhdl_only\ : in std_logic;
            o_simple : out std_logic;
            \o_extended_in_?both\ : out std_logic;
            \o_extended_in_$vhdl_only\ : out std_logic
        );
    end entity;

    architecture rtl of naming is

        signal signal_select : std_logic;
        signal signal_select_1 : std_logic;
        signal signal_wire : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_wire_2 : std_logic;
        signal signal_inst : std_logic_vector(2 downto 0);
        signal signal_select_2 : std_logic;

    begin

        signal_select <= signal_inst(2);
        signal_select_1 <= signal_inst(1);
        signal_wire <= \i_extended_in_$vhdl_only\;
        signal_wire_1 <= \i_extended_in_?both\;
        signal_wire_2 <= i_simple;
        test_naming: entity work.test_naming (rtl)
            port map ( i_simple => signal_wire_2,
                       \i_extended_in_?both\ => signal_wire_1,
                       \i_extended_in_$vhdl_only\ => signal_wire,
                       o_simple => signal_inst(0),
                       \o_extended_in_?both\ => signal_inst(1),
                       \o_extended_in_$vhdl_only\ => signal_inst(2) );
        signal_select_2 <= signal_inst(0);
        o_simple <= signal_select_2;
        \o_extended_in_?both\ <= signal_select_1;
        \o_extended_in_$vhdl_only\ <= signal_select;

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
