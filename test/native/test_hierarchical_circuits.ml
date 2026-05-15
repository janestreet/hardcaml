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

        wire signal_inst;
        wire signal_wire;
        wire signal_inst_1;
        wire signal_wire_1;
        wire signal_or;
        inner
            the_inner
            ( .a(a),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        inner
            the_inner_1
            ( .a(a),
              .b(signal_inst_1) );
        assign signal_wire_1 = signal_inst_1;
        assign signal_or = signal_wire_1 | signal_wire;
        assign b = signal_or;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        wire signal_inst;
        wire signal_wire;
        middle
            the_middle
            ( .a(a),
              .b(signal_inst) );
        assign signal_wire = signal_inst;
        assign b = signal_wire;

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

        signal signal_inst : std_logic;
        signal signal_wire : std_logic;
        signal signal_inst_1 : std_logic;
        signal signal_wire_1 : std_logic;
        signal signal_or : std_logic;

    begin

        the_inner: entity work.inner (rtl)
            port map ( a => a,
                       b => signal_inst );
        signal_wire <= signal_inst;
        the_inner_1: entity work.inner (rtl)
            port map ( a => a,
                       b => signal_inst_1 );
        signal_wire_1 <= signal_inst_1;
        signal_or <= signal_wire_1 or signal_wire;
        b <= signal_or;

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

        signal signal_inst : std_logic;
        signal signal_wire : std_logic;

    begin

        the_middle: entity work.middle (rtl)
            port map ( a => a,
                       b => signal_inst );
        signal_wire <= signal_inst;
        b <= signal_wire;

    end architecture;
    |}];
  return ()
;;
