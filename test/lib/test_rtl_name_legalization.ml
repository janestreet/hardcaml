open! Import

let%expect_test "rtl name legalization" =
  let verilog = Rtl.Name.create (module Rtl.Name.Verilog) in
  let vhdl = Rtl.Name.create (module Rtl.Name.Vhdl) in
  let show name =
    [%message
      ""
        ~verilog:(Rtl.Name.legalize verilog name : string)
        ~vhdl:(Rtl.Name.legalize vhdl name : string)]
    |> print_s
  in
  require_does_raise ~cr:CR_someday [%here] (fun () -> show "");
  [%expect {| "[Rtl_name] string is empty" |}];
  (* underscore really is a valid verilog name... *)
  show "_";
  [%expect {|
    ((verilog _)
     (vhdl    hc__))
    |}];
  show "1";
  [%expect {|
    ((verilog _1)
     (vhdl    hc_1))
    |}];
  show "_1";
  [%expect {|
    ((verilog _1)
     (vhdl    hc__1))
    |}];
  show "$";
  [%expect {|
    ((verilog _$)
     (vhdl    hc__))
    |}];
  show "foo!\"£$%^&*()\"";
  [%expect {|
    ((verilog foo____$_______)
     (vhdl    foo____________))
    |}]
;;
