open Core
open Async
open Hardcaml
open Expect_test_helpers_async

let outer = Hardcaml_test.Test_hierarchical_circuits.outer

let%expect_test "To_file" =
  let%bind () =
    within_temp_dir (fun () ->
      let database = Circuit_database.create () in
      let circuit = outer ~db:database ~cause_exn:false ~share:true in
      let%bind () =
        Writer.save
          "single_file.v"
          ~contents:
            (Rtl.create ~database Verilog [ circuit ]
             |> Rtl.full_hierarchy
             |> Rope.to_string)
      in
      system "ls *.v; cat *.v")
  in
  [%expect
    {|
    single_file.v
    module inner (
        a,
        b
    );

        input a;
        output b;

        assign b = a;

    endmodule
    module middle (
        a,
        b
    );

        input a;
        output b;

        wire _5;
        wire _1;
        wire _6;
        wire _3;
        wire _7;
        inner
            the_inner
            ( .a(a),
              .b(_5) );
        assign _1 = _5;
        inner
            the_inner_1
            ( .a(a),
              .b(_6) );
        assign _3 = _6;
        assign _7 = _3 | _1;
        assign b = _7;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        wire _4;
        wire _2;
        middle
            the_middle
            ( .a(a),
              .b(_4) );
        assign _2 = _4;
        assign b = _2;

    endmodule
    |}];
  return ()
;;

let%expect_test "In_directory" =
  let%bind () =
    within_temp_dir (fun () ->
      let database = Circuit_database.create () in
      let circuit = outer ~db:database ~cause_exn:false ~share:true in
      let rtl = Rtl.create ~database Vhdl [ circuit ] in
      let output circuits =
        Deferred.List.iter ~how:`Sequential circuits ~f:(fun circuit ->
          Writer.save
            (Rtl.Circuit_instance.module_name circuit ^ ".vhd")
            ~contents:(Rtl.Circuit_instance.rtl circuit |> Rope.to_string))
      in
      let%bind () = output (Rtl.Hierarchical_circuits.subcircuits rtl) in
      let%bind () = output (Rtl.Hierarchical_circuits.top rtl) in
      system "ls *.vhd; cat *.vhd")
  in
  [%expect
    {|
    inner.vhd
    middle.vhd
    outer.vhd
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity inner is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of inner is


    begin

        b <= a;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity middle is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of middle is

        signal \_5\ : std_logic;
        signal \_1\ : std_logic;
        signal \_6\ : std_logic;
        signal \_3\ : std_logic;
        signal \_7\ : std_logic;

    begin

        the_inner: entity work.inner (rtl)
            port map ( a => a,
                       b => \_5\ );
        \_1\ <= \_5\;
        the_inner_1: entity work.inner (rtl)
            port map ( a => a,
                       b => \_6\ );
        \_3\ <= \_6\;
        \_7\ <= \_3\ or \_1\;
        b <= \_7\;

    end architecture;
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity outer is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of outer is

        signal \_4\ : std_logic;
        signal \_2\ : std_logic;

    begin

        the_middle: entity work.middle (rtl)
            port map ( a => a,
                       b => \_4\ );
        \_2\ <= \_4\;
        b <= \_2\;

    end architecture;
    |}];
  return ()
;;
