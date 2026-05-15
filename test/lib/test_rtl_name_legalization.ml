open! Import

let%expect_test "rtl name legalization" =
  let show name =
    let alignment = 15 in
    let padding n = String.init n ~f:(fun _ -> ' ') in
    print_endline [%string {|%{padding alignment} %{name}|}];
    List.iter Rtl.Language.all ~f:(fun language ->
      let scope = Rtl.Name.Scope.create () in
      let name =
        Rtl.Name.Scope.mangle_name scope name
        |> Rtl.Name.legalize ~language
        |> Rtl.Name.For_backend.to_string
      in
      let lang = Rtl.Language.to_string language in
      let padding = padding (max 0 (alignment - String.length lang)) in
      print_endline [%string {|%{lang}%{padding} %{name}|}]);
    print_endline ""
  in
  List.iter
    [ ""; "foo!\"£$%^&*()\""; [%string {|\|}]; [%string {|\\|}]; "a name with spaces" ]
    ~f:(fun invalid_name -> require_does_raise (fun () -> show invalid_name));
  [%expect
    {|
    "[Rtl_name] string must not be empty"
                    foo!"£$%^&*()"
    ("[Rtl_name]s must only contain printable characters and may not contain spaces or back slashes"
     (identifier "foo!\"\194\163$%^&*()\""))
                    \
    ("[Rtl_name]s must only contain printable characters and may not contain spaces or back slashes"
     (identifier \))
                    \\
    ("[Rtl_name]s must only contain printable characters and may not contain spaces or back slashes"
     (identifier "\\\\"))
                    a name with spaces
    ("[Rtl_name]s must only contain printable characters and may not contain spaces or back slashes"
     (identifier "a name with spaces"))
    |}];
  List.iter
    [ "_" (* underscore really is a valid verilog name... *)
    ; "__"
    ; "1"
    ; "_1"
    ; "1_"
    ; "$"
    ; "foo!\"$%^&*()\""
    ; "entity"
    ; "module"
    ; "for"
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
    Verilog         \1
    Systemverilog   \1
    Vhdl            \1\

                    _1
    Verilog         _1
    Systemverilog   _1
    Vhdl            \_1\

                    1_
    Verilog         \1_
    Systemverilog   \1_
    Vhdl            \1_\

                    $
    Verilog         \$
    Systemverilog   \$
    Vhdl            \$\

                    foo!"$%^&*()"
    Verilog         \foo!"$%^&*()"
    Systemverilog   \foo!"$%^&*()"
    Vhdl            \foo!"$%^&*()"\

                    entity
    Verilog         entity
    Systemverilog   entity
    Vhdl            \entity\

                    module
    Verilog         \module
    Systemverilog   \module
    Vhdl            module

                    for
    Verilog         \for
    Systemverilog   \for
    Vhdl            \for\
    |}]
;;
