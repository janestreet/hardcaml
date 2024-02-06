open Core
open Hardcaml
module Unix = Core_unix

let patdiff s1 s2 = Expect_test_patdiff.print_patdiff ~keep_ws:true s1 s2

let hdl_old blackbox lang circuit =
  let buffer = Buffer.create 1024 in
  match (lang : Rtl.Language.t) with
  | Verilog ->
    ignore
      (Rtl.Deprecated.Verilog.write blackbox (Buffer.add_string buffer) circuit
        : Rtl.Signals_name_map.t);
    Buffer.contents buffer
  | Vhdl ->
    ignore
      (Rtl.Deprecated.Vhdl.write blackbox (Buffer.add_string buffer) circuit
        : Rtl.Signals_name_map.t);
    Buffer.contents buffer
;;

let hdl_new blackbox (lang : Rtl.Language.t) circuit =
  let buffer_hdl_new = Buffer.create 1024 in
  (match lang with
   | Verilog ->
     let ast =
       Rtl.Ast.of_circuit ~blackbox (Rtl.Name.create (module Rtl.Name.Verilog)) circuit
     in
     (* for coverage *)
     ignore (Rtl.Ast.sexp_of_t ast : Sexp.t);
     Rtl.Verilog.to_buffer buffer_hdl_new ast
   | Vhdl ->
     let ast =
       Rtl.Ast.of_circuit ~blackbox (Rtl.Name.create (module Rtl.Name.Vhdl)) circuit
     in
     (* for coverage *)
     ignore (Rtl.Ast.sexp_of_t ast : Sexp.t);
     Rtl.Vhdl.to_buffer buffer_hdl_new ast);
  Buffer.contents buffer_hdl_new
;;

let diff lang circuit =
  let hdl_old = hdl_old false lang circuit in
  let hdl_new = hdl_new false lang circuit in
  if not (String.equal hdl_old hdl_new) then patdiff hdl_old hdl_new
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
  let hdl = hdl_new blackbox Verilog circuit in
  analyse ?quiet Verilog circuit hdl;
  if show then Out_channel.print_string hdl;
  let hdl = hdl_new blackbox Vhdl circuit in
  analyse ?quiet Vhdl circuit hdl;
  if show then Out_channel.print_string hdl
;;

let diff_and_analyse ?quiet ?(show = false) ?(blackbox = false) lang circuit =
  let hdl_old = hdl_old blackbox lang circuit in
  let hdl_new = hdl_new blackbox lang circuit in
  if not (String.equal hdl_old hdl_new) then patdiff hdl_old hdl_new;
  if show then Out_channel.print_string hdl_new;
  analyse ?quiet lang circuit hdl_new
;;

let diff_and_analyse_vhdl_and_verilog ?quiet ?show ?blackbox circuit =
  diff_and_analyse ?quiet ?show ?blackbox Verilog circuit;
  diff_and_analyse ?quiet ?show ?blackbox Vhdl circuit
;;
