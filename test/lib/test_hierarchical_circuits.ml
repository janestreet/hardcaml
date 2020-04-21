(* Test generation of hierarchical circuits. *)
open! Import
open Signal

(* [inner] *)
let inner ~cause_exn ~db ~share =
  let a = input "a" 1 in
  let b = output (if cause_exn then "a" else "b") a in
  let circ = Circuit.create_exn ~name:"inner" [ b ] in
  let name = Circuit_database.insert ~share db circ in
  name
;;

(* [middle] *)
let middle ~cause_exn ~db ~share =
  let inner_name1 = inner ~cause_exn ~db ~share in
  let inner_name2 = inner ~cause_exn ~db ~share in
  (* create 2 instantiations of [inner] so we can demonstrate the effect of sharing. *)
  let a = input "a" 1 in
  let inst =
    Instantiation.create () ~name:inner_name1 ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
  in
  let b1 = inst#o "b" in
  let inst =
    Instantiation.create () ~name:inner_name2 ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
  in
  let b2 = inst#o "b" in
  let circ = Circuit.create_exn ~name:"middle" [ output "b" (b1 |: b2) ] in
  let name = Circuit_database.insert ~share db circ in
  name
;;

(* [outer] *)
let outer ~cause_exn ~db ~share =
  let middle_name = middle ~cause_exn ~db ~share in
  let a = input "a" 1 in
  let inst =
    Instantiation.create () ~name:middle_name ~inputs:[ "a", a ] ~outputs:[ "b", 1 ]
  in
  let b = output "b" (inst#o "b") in
  Circuit.create_exn ~name:"outer" [ b ]
;;

let create ~cause_exn ~share =
  let db = Circuit_database.create () in
  let outer_circ = outer ~cause_exn ~db ~share in
  print_s [%message "" ~circuit_database:(db : Circuit_database.t)];
  print_string "\nVerilog\n--------\n";
  Rtl.print ~database:db Verilog outer_circ
;;

let%expect_test "generate hierarchy without sharing" =
  create ~cause_exn:false ~share:false;
  [%expect
    {|
    (circuit_database (
      (inner  inner)
      (inner  inner_0)
      (middle middle)))

    Verilog
    --------
    module inner (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */

        /* logic */

        /* aliases */

        /* output assignments */
        assign b = a;

    endmodule
    module inner_0 (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */

        /* logic */

        /* aliases */

        /* output assignments */
        assign b = a;

    endmodule
    module middle (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */
        wire _6;
        wire _4;
        wire _7;

        /* logic */
        inner_0
            the_inner_0
            ( .a(a), .b(_6) );
        inner
            the_inner
            ( .a(a), .b(_4) );
        assign _7 = _4 | _6;

        /* aliases */

        /* output assignments */
        assign b = _7;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */
        wire _4;

        /* logic */
        middle
            the_middle
            ( .a(a), .b(_4) );

        /* aliases */

        /* output assignments */
        assign b = _4;

    endmodule |}]
;;

let%expect_test "generate hierarchy with sharing" =
  create ~cause_exn:false ~share:true;
  [%expect
    {|
    (circuit_database (
      (inner  inner)
      (middle middle)))

    Verilog
    --------
    module inner (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */

        /* logic */

        /* aliases */

        /* output assignments */
        assign b = a;

    endmodule
    module middle (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */
        wire _6;
        wire _4;
        wire _7;

        /* logic */
        inner
            the_inner
            ( .a(a), .b(_6) );
        inner
            the_inner_0
            ( .a(a), .b(_4) );
        assign _7 = _4 | _6;

        /* aliases */

        /* output assignments */
        assign b = _7;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */
        wire _4;

        /* logic */
        middle
            the_middle
            ( .a(a), .b(_4) );

        /* aliases */

        /* output assignments */
        assign b = _4;

    endmodule |}]
;;

let%expect_test "generate hierarchy exn" =
  show_raise (fun () -> create ~cause_exn:true ~share:true);
  [%expect
    {|
    (circuit_database (
      (inner  inner)
      (middle middle)))

    Verilog
    --------
    (raised (
      "Error while writing circuit"
      (circuit_name inner)
      (hierarchy_path (outer middle inner))
      (output ((language Verilog) (mode (To_channel <stdout>))))
      (exn (
        "port name has already been defined or matches a reserved identifier"
        (port (
          wire
          (names (a))
          (width   1)
          (data_in a))))))) |}]
;;

open Async
open Expect_test_helpers_async

let%expect_test "To_file" =
  let%bind () =
    within_temp_dir (fun () ->
      let database = Circuit_database.create () in
      let circuit = outer ~db:database ~cause_exn:false ~share:true in
      Rtl.output Verilog circuit ~output_mode:(To_file "single_file.v") ~database;
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

        /* signal declarations */

        /* logic */

        /* aliases */

        /* output assignments */
        assign b = a;

    endmodule
    module middle (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */
        wire _6;
        wire _4;
        wire _7;

        /* logic */
        inner
            the_inner
            ( .a(a), .b(_6) );
        inner
            the_inner_0
            ( .a(a), .b(_4) );
        assign _7 = _4 | _6;

        /* aliases */

        /* output assignments */
        assign b = _7;

    endmodule
    module outer (
        a,
        b
    );

        input a;
        output b;

        /* signal declarations */
        wire _4;

        /* logic */
        middle
            the_middle
            ( .a(a), .b(_4) );

        /* aliases */

        /* output assignments */
        assign b = _4;

    endmodule |}]
;;

let%expect_test "In_directory" =
  let%bind () =
    within_temp_dir (fun () ->
      let database = Circuit_database.create () in
      let circuit = outer ~db:database ~cause_exn:false ~share:true in
      Rtl.output Vhdl circuit ~output_mode:(In_directory ".") ~database;
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

    begin

        -- logic

        -- aliases

        -- output assignments
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
        signal hc_6 : std_logic;
        signal hc_4 : std_logic;
        signal hc_7 : std_logic;

    begin

        -- logic
        the_inner: entity work.inner (rtl)
            port map ( a => a, b => hc_6 );
        the_inner_0: entity work.inner (rtl)
            port map ( a => a, b => hc_4 );
        hc_7 <= hc_sl(hc_uns(hc_4) or hc_uns(hc_6));

        -- aliases

        -- output assignments
        b <= hc_7;

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
        signal hc_4 : std_logic;

    begin

        -- logic
        the_middle: entity work.middle (rtl)
            port map ( a => a, b => hc_4 );

        -- aliases

        -- output assignments
        b <= hc_4;

    end architecture; |}]
;;
