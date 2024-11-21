open Core

let attribute =
  Command.Arg_type.create (function
    | "auto" -> `auto
    | "none" -> `none
    | "sequential" -> `sequential
    | "gray" -> `gray
    | "onehot" -> `one_hot
    | "johnson" -> `johnson
    | _ -> raise_s [%message "Unknonw vivado encoding"])
;;

let encoding =
  Command.Arg_type.create (function
    | "binary" -> (Binary : Hardcaml.Always.State_machine.Encoding.t)
    | "onehot" -> Onehot
    | "gray" -> Gray
    | _ -> raise_s [%message "Unknown hardcaml encoding"])
;;

let language =
  Command.Arg_type.create (function
    | "verilog" -> (Verilog : Hardcaml.Rtl.Language.t)
    | "vhdl" -> Vhdl
    | _ -> raise_s [%message "Unknown RTL generator"])
;;

let () =
  Command_unix.run
    (Command.basic
       ~summary:""
       [%map_open.Command
         let attribute = flag "-attribute" (optional attribute) ~doc:""
         and encoding = flag "-encoding" (optional encoding) ~doc:""
         and lang =
           flag
             "-language"
             (optional_with_default Hardcaml.Rtl.Language.Verilog language)
             ~doc:""
         and name = anon ("CIRCUIT_NAME" %: string) in
         fun () -> Hardcaml_test.Test_statemachine.print ?attribute ?encoding name lang])
;;
