open Core
open Hardcaml

let command =
  Command.basic
    ~summary:""
    [%map_open.Command
      let address_width = flag "-address-width" (required int) ~doc:""
      and data_width = flag "-data-width" (required int) ~doc:""
      and read_ports = flag "-read-ports" (required int) ~doc:""
      and vhdl = flag "-vhdl" no_arg ~doc:"" in
      fun () ->
        let spec = Signal.Reg_spec.create ~clock:(Signal.input "clock" 1) () in
        let rom =
          Signal.rom
            ~read_addresses:
              (Array.init read_ports ~f:(fun i ->
                 Signal.input [%string "read_address%{i#Int}"] address_width))
            (Array.init (1 lsl address_width) ~f:(fun _ -> Bits.random ~width:data_width))
        in
        let outputs =
          Array.to_list rom
          |> List.map ~f:(Signal.reg spec)
          |> List.mapi ~f:(fun i -> Signal.output [%string "q%{i#Int}"])
        in
        let circuit = Circuit.create_exn ~name:"rom" outputs in
        Rtl.print (if vhdl then Vhdl else Verilog) circuit]
;;

let () = Command_unix.run command
