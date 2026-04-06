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
        wire _2;
        wire _6;
        assign \module  = ~ _2;
        assign select = _2 + \module ;
        assign _2 = a;
        assign _6 = _2 + select;
        assign b = _6;

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
        signal \_2\ : std_logic;
        signal \_6\ : std_logic;

    begin

        module <= not \_2\;
        \select\ <= (unsigned(std_logic_vector'("" & \_2\)) + unsigned(std_logic_vector'("" & module))) ?= "1";
        \_2\ <= a;
        \_6\ <= (unsigned(std_logic_vector'("" & \_2\)) + unsigned(std_logic_vector'("" & \select\))) ?= "1";
        b <= \_6\;

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
        wire _2;
        wire _6;
        assign some_name = ~ _2;
        assign Some_name_1 = _2 + some_name;
        assign _2 = a;
        assign _6 = _2 + Some_name_1;
        assign b = _6;

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
        signal \_2\ : std_logic;
        signal \_6\ : std_logic;

    begin

        some_name <= not \_2\;
        Some_name_1 <= (unsigned(std_logic_vector'("" & \_2\)) + unsigned(std_logic_vector'("" & some_name))) ?= "1";
        \_2\ <= a;
        \_6\ <= (unsigned(std_logic_vector'("" & \_2\)) + unsigned(std_logic_vector'("" & Some_name_1))) ?= "1";
        b <= \_6\;

    end architecture;
    |}]
;;
