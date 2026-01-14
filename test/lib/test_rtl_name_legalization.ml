open! Import

let%expect_test "rtl name legalization" =
  let show name =
    let alignment = 15 in
    let padding n = String.init n ~f:(fun _ -> ' ') in
    print_endline [%string {|%{padding alignment} %{name}|}];
    List.iter Rtl.Language.all ~f:(fun lang ->
      let scope = Rtl.Name.create lang in
      let lang = Rtl.Language.to_string lang in
      let padding = padding (max 0 (alignment - String.length lang)) in
      print_endline [%string {|%{lang}%{padding} %{Rtl.Name.mangle_name scope name }|}]);
    print_endline ""
  in
  require_does_raise ~cr:CR_someday (fun () -> show "");
  [%expect {| "[Rtl_name.legalize] string is empty" |}];
  List.iter
    [ "_" (* underscore really is a valid verilog name... *)
    ; "__"
    ; "1"
    ; "_1"
    ; "1_"
    ; "$"
    ; "foo!\"£$%^&*()\""
    ; [%string {|\|}]
    ; [%string {|\\|}]
    ; "a name with spaces"
    ]
    ~f:show;
  [%expect
    {|
                    _
    Verilog         _
    Systemverilog   _
    Vhdl            \_\

                    __
    Verilog         __
    Systemverilog   __
    Vhdl            \__\

                    1
    Verilog         _1
    Systemverilog   _1
    Vhdl            \1\

                    _1
    Verilog         _1
    Systemverilog   _1
    Vhdl            \_1\

                    1_
    Verilog         _1_
    Systemverilog   _1_
    Vhdl            \1_\

                    $
    Verilog         _$
    Systemverilog   _$
    Vhdl            \$\

                    foo!"£$%^&*()"
    Verilog         foo____$_______
    Systemverilog   foo____$_______
    Vhdl            \foo!"£$%^&*()"\

                    \
    Verilog         __
    Systemverilog   __
    Vhdl            \\\\

                    \\
    Verilog         ___
    Systemverilog   ___
    Vhdl            \\\\\\

                    a name with spaces
    Verilog         a_name_with_spaces
    Systemverilog   a_name_with_spaces
    Vhdl            \a_name_with_spaces\
    |}]
;;
