open! Import

let%expect_test "rtl name legalization" =
  let verilog = (module Rtl.Name.Verilog : Rtl.Name.Language) in
  let vhdl = (module Rtl.Name.Vhdl : Rtl.Name.Language) in
  let legalize (module Lang : Rtl.Name.Language) s = Lang.legalize s in
  let show name =
    [%message
      "" ~verilog:(legalize verilog name : string) ~vhdl:(legalize vhdl name : string)]
    |> print_s
  in
  require_does_raise ~cr:CR_someday (fun () -> show "");
  [%expect {| "[Rtl_name.legalize] string is empty" |}];
  (* underscore really is a valid verilog name... *)
  show "_";
  [%expect
    {|
    ((verilog _)
     (vhdl    hc_hc))
    |}];
  show "1";
  [%expect
    {|
    ((verilog _1)
     (vhdl    hc_1))
    |}];
  show "_1";
  [%expect
    {|
    ((verilog _1)
     (vhdl    hc_1))
    |}];
  show "$";
  [%expect
    {|
    ((verilog _$)
     (vhdl    hc_hc))
    |}];
  show "foo!\"£$%^&*()\"";
  [%expect
    {|
    ((verilog foo____$_______)
     (vhdl    foo_hc))
    |}]
;;
