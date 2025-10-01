open Core
open Hardcaml
module Unix = Core_unix

let hdl ?database ?config blackbox (lang : Rtl.Language.t) circuit =
  Rtl.create ?database ?config lang [ circuit ]
  |> (if blackbox then Rtl.top_levels_as_blackboxes else Rtl.full_hierarchy)
  |> Rope.to_string
;;

let iverilog = Tools_config.iverilog
let ghdl = Tools_config.ghdl

let redirect quiet command =
  if quiet then command ^ " 2> /dev/null > /dev/null" else command
;;

let analyse_verilog ?(quiet = false) circuit verilog =
  let vlog_file = Filename_unix.temp_file (Circuit.name circuit) ".v" in
  Out_channel.write_all vlog_file ~data:verilog;
  let result =
    (* compile with systemverilog (ieee1800-2012), which should be compatible with
       different hardcaml generation options. *)
    let command = [%string "%{iverilog} -t null -g2012 %{vlog_file}"] in
    Unix.system (redirect quiet command)
  in
  Unix.unlink vlog_file;
  match result with
  | Ok () -> ()
  | error_code ->
    print_s [%message "Icarus Verilog failed with" (error_code : Unix.Exit_or_signal.t)]
;;

let analyse_vhdl ?(quiet = false) circuit vhdl =
  let vhd_file = Filename_unix.temp_file (Circuit.name circuit) ".vhd" in
  Out_channel.write_all vhd_file ~data:vhdl;
  let result =
    let command =
      [%string "%{ghdl} -s --std=08 --workdir=%{Filename.temp_dir_name} %{vhd_file}"]
    in
    Unix.system (redirect quiet command)
  in
  Unix.unlink vhd_file;
  match result with
  | Ok () -> ()
  | error_code ->
    print_s [%message "GHDL failed with" (error_code : Unix.Exit_or_signal.t)]
;;

let analyse ?quiet lang circuit hdl =
  match (lang : Rtl.Language.t) with
  | Verilog | Systemverilog -> analyse_verilog ?quiet circuit hdl
  | Vhdl -> analyse_vhdl ?quiet circuit hdl
;;

let analyse_vhdl_and_verilog
  ?quiet
  ?(show = false)
  ?(blackbox = false)
  ?database
  ?config
  circuit
  =
  let hdl' = hdl ?database ?config blackbox Verilog circuit in
  analyse ?quiet Verilog circuit hdl';
  if show then Out_channel.print_string hdl';
  let hdl' = hdl ?database ?config blackbox Vhdl circuit in
  analyse ?quiet Vhdl circuit hdl';
  if show then Out_channel.print_string hdl'
;;
