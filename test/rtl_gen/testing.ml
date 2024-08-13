open Core
open Hardcaml
module Unix = Core_unix

let hdl blackbox (lang : Rtl.Language.t) circuit =
  let buffer_hdl = Buffer.create 1024 in
  (match lang with
   | Verilog ->
     let ast = Rtl.Ast.of_circuit ~blackbox ~language:lang circuit in
     (* for coverage *)
     ignore (Rtl.Ast.sexp_of_t ast : Sexp.t);
     Rtl.Verilog.to_buffer buffer_hdl ast
   | Vhdl ->
     let ast = Rtl.Ast.of_circuit ~blackbox ~language:lang circuit in
     (* for coverage *)
     ignore (Rtl.Ast.sexp_of_t ast : Sexp.t);
     Rtl.Vhdl.to_buffer buffer_hdl ast);
  Buffer.contents buffer_hdl
;;

let iverilog = Setup.iverilog

let redirect quiet command =
  if quiet then command ^ " 2> /dev/null > /dev/null" else command
;;

let analyse_verilog ?(quiet = false) circuit verilog =
  let vlog_file = Filename_unix.temp_file (Circuit.name circuit) ".v" in
  Out_channel.write_all vlog_file ~data:verilog;
  let result =
    let command = [%string "%{iverilog} -t null %{vlog_file}"] in
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
    let command = [%string "ghdl -s --workdir=%{Filename.temp_dir_name} %{vhd_file}"] in
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
  | Verilog -> analyse_verilog ?quiet circuit hdl
  | Vhdl -> analyse_vhdl ?quiet circuit hdl
;;

let analyse_vhdl_and_verilog ?quiet ?(show = false) ?(blackbox = false) circuit =
  let hdl' = hdl blackbox Verilog circuit in
  analyse ?quiet Verilog circuit hdl';
  if show then Out_channel.print_string hdl';
  let hdl' = hdl blackbox Vhdl circuit in
  analyse ?quiet Vhdl circuit hdl';
  if show then Out_channel.print_string hdl'
;;
