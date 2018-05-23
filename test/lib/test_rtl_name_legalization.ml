open! Import

let%expect_test "rtl name legalization" =
  let show name =
    [%message
      ""
        ~verilog:(Rtl.Language.legalize_identifier Verilog name : string)
        ~vhdl:   (Rtl.Language.legalize_identifier Vhdl    name : string)]
    |> print_s
  in
  require_does_not_raise ~cr:CR_soon [%here] (fun () -> show "");
  [%expect {|
    ("unexpectedly raised" (Invalid_argument "index out of bounds")) |}];
  (* underscore really is a valid verilog name... *)
  show "_";
  [%expect {|
    ((verilog _)
     (vhdl    hc__)) |}];
  show "1";
  [%expect {|
    ((verilog _1)
     (vhdl    hc_1)) |}];
  show "_1";
  [%expect {|
    ((verilog _1)
     (vhdl    hc__1)) |}];
  show "$";
  [%expect {|
    ((verilog _$)
     (vhdl    hc__)) |}];
  show "foo!\"£$%^&*()\"";
  [%expect {|
    ((verilog foo____$_______)
     (vhdl    foo____________)) |}];
;;
