open Core
open Hardcaml
open Signal

let%expect_test "blackboxes" =
  let circuit = Circuit.create_exn ~name:"blackboxes" [ output "y" ~:(input "x" 1) ] in
  Testing.analyse_vhdl_and_verilog ~blackbox:true ~show:true circuit;
  [%expect
    {|
    module blackboxes (
        x,
        y
    );

        input x;
        output y;


    endmodule
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity blackboxes is
        port (
            x : in std_logic;
            y : out std_logic
        );
    end entity;

    architecture rtl of blackboxes is


    begin


    end architecture;
    |}]
;;
