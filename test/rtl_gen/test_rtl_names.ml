open Core
open Hardcaml

let%expect_test "test keywords" =
  let module I = struct
    type 'a t = { a : 'a } [@@deriving hardcaml]
  end
  in
  let module O = struct
    type 'a t = { b : 'a } [@@deriving hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"names" (fun { a } ->
      let open Signal in
      let tmp1 = ~:a -- "module" (* Verilog keyword *) in
      let tmp2 = a +: tmp1 -- "select" (* VHDL keywork *) in
      let b = a +: tmp2 in
      { b })
  in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module names (
        a,
        b
    );

        input a;
        output b;

        wire \module ;
        wire select;
        wire signal_wire;
        wire signal_add;
        assign \module  = ~ signal_wire;
        assign select = signal_wire + \module ;
        assign signal_wire = a;
        assign signal_add = signal_wire + select;
        assign b = signal_add;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity names is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of names is

        signal module : std_logic;
        signal \select\ : std_logic;
        signal signal_wire : std_logic;
        signal signal_add : std_logic;

    begin

        module <= not signal_wire;
        \select\ <= (unsigned(std_logic_vector'("" & signal_wire)) + unsigned(std_logic_vector'("" & module))) ?= "1";
        signal_wire <= a;
        signal_add <= (unsigned(std_logic_vector'("" & signal_wire)) + unsigned(std_logic_vector'("" & \select\))) ?= "1";
        b <= signal_add;

    end architecture;
    |}]
;;

let%expect_test "test case sensitivity" =
  let module I = struct
    type 'a t = { a : 'a } [@@deriving hardcaml]
  end
  in
  let module O = struct
    type 'a t = { b : 'a } [@@deriving hardcaml]
  end
  in
  let module C = Circuit.With_interface (I) (O) in
  let circuit =
    C.create_exn ~name:"names" (fun { a } ->
      let open Signal in
      let tmp1 = ~:a -- "some_name" in
      let tmp2 = a +: tmp1 -- "Some_name" in
      let b = a +: tmp2 in
      { b })
  in
  Testing.analyse_vhdl_and_verilog ~show:true circuit;
  [%expect
    {|
    module names (
        a,
        b
    );

        input a;
        output b;

        wire some_name;
        wire Some_name_1;
        wire signal_wire;
        wire signal_add;
        assign some_name = ~ signal_wire;
        assign Some_name_1 = signal_wire + some_name;
        assign signal_wire = a;
        assign signal_add = signal_wire + Some_name_1;
        assign b = signal_add;

    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity names is
        port (
            a : in std_logic;
            b : out std_logic
        );
    end entity;

    architecture rtl of names is

        signal some_name : std_logic;
        signal Some_name_1 : std_logic;
        signal signal_wire : std_logic;
        signal signal_add : std_logic;

    begin

        some_name <= not signal_wire;
        Some_name_1 <= (unsigned(std_logic_vector'("" & signal_wire)) + unsigned(std_logic_vector'("" & some_name))) ?= "1";
        signal_wire <= a;
        signal_add <= (unsigned(std_logic_vector'("" & signal_wire)) + unsigned(std_logic_vector'("" & Some_name_1))) ?= "1";
        b <= signal_add;

    end architecture;
    |}]
;;
