open! Import

type delta_message =
  { sets : (string * string) list
  ; gets : string list
  ; delta_time : int64
  }

type init_message = string list

type control_message =
  | Finish
  | Run of delta_message

type response_message = (string * string) list

let net_addr = "localhost"
let net_port = 10101

module Comms = struct
  open Unix

  let empty = ""

  let create_client server port =
    let sock = socket PF_INET SOCK_STREAM 0 in
    let server_addr = ADDR_INET ((gethostbyname server).h_addr_list.(0), port) in
    connect sock server_addr;
    sock
  ;;

  let create_server client port =
    let sock = socket PF_INET SOCK_STREAM 0 in
    setsockopt sock SO_REUSEADDR true;
    let client_addr = ADDR_INET ((gethostbyname client).h_addr_list.(0), port) in
    bind sock client_addr;
    listen sock 1;
    sock
  ;;

  let accept_client sock = fst (accept sock)

  (* send value stored in byte buffer *)
  let send sock bytes = write sock (Bytes.of_string bytes) 0 (String.length bytes)

  (* recv marshalled value to a buffer *)
  let recv sock =
    let header = Bytes.create Marshal.header_size in
    if Marshal.header_size <> read sock header 0 Marshal.header_size
    then failwith "recv_marshalled Marshal.header_size";
    let data_size = Marshal.data_size header 0 in
    let data = Bytes.create data_size in
    if data_size <> read sock data 0 data_size
    then failwith "recv_marshalled Marshal.data_size";
    String.concat ~sep:empty [ Bytes.to_string header; Bytes.to_string data ]
  ;;

  let send_string socket str = send socket (Marshal.to_string str [])
  let recv_string socket = (Marshal.from_string (recv socket) 0 : string)

  let recv_string_is socket expected =
    let got = recv_string socket in
    if not (String.equal got expected)
    then failwith ("recv_string_is expected '" ^ expected ^ "' got '" ^ got ^ "'")
  ;;
end

let control server message =
  ignore (Comms.send server (Marshal.to_string message []) : int);
  match message with
  | Finish -> []
  | Run { gets; _ } when List.is_empty gets -> []
  | _ -> (Marshal.from_string (Comms.recv server) 0 : response_message)
;;

let instance_name name = "the_hardcaml_" ^ name

let write_testbench ?dump_file ~name ~inputs ~outputs os =
  let declare net s =
    let width = snd s in
    os ("  " ^ net ^ " ");
    if width > 1
    then (
      os "[";
      os (Int.to_string (width - 1));
      os ":0] ");
    os (fst s);
    os ";\n"
  in
  os ("module " ^ name ^ "_hardcaml_testbench;\n");
  List.iter inputs ~f:(declare "reg");
  List.iter outputs ~f:(declare "wire");
  (match dump_file with
   | Some dump_file ->
     os "  initial begin\n";
     os ("    $dumpfile(\"" ^ dump_file ^ "\");\n");
     os ("    $dumpvars(0, " ^ instance_name name ^ ");\n");
     os "  end\n"
   | None -> ());
  os ("  " ^ name ^ " " ^ instance_name name ^ " (");
  let ports =
    List.map (inputs @ outputs) ~f:(fun s -> "." ^ fst s ^ "(" ^ fst s ^ ")")
  in
  os (String.concat ~sep:", " ports);
  os ");\n";
  os "endmodule"
;;

let write_testbench_from_circuit ?dump_file os circuit =
  let cname = Circuit.name circuit in
  let name s = List.hd_exn (Signal.names s) in
  let inputs = List.map (Circuit.inputs circuit) ~f:(fun s -> name s, Signal.width s) in
  let outputs =
    List.map (Circuit.outputs circuit) ~f:(fun s -> name s, Signal.width s)
  in
  write_testbench ?dump_file ~name:cname ~inputs ~outputs os
;;

let compile verilog vvp =
  match Unix.system ("iverilog -o " ^ vvp ^ " " ^ String.concat ~sep:" " verilog) with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith "Failed to compile verilog to vvp"
;;

let derive_clocks_and_resets circuit =
  let seq_elts =
    Signal_graph.filter (Circuit.signal_graph circuit) ~f:(fun s ->
      Signal.is_reg s || Signal.is_mem s)
  in
  let clocks_and_resets =
    List.map seq_elts ~f:(function
      | Reg { register = r; _ } -> [ r.reg_clock ], r.reg_reset
      | Mem { register = r; _ } -> [ r.reg_clock ], r.reg_reset
      | Multiport_mem { write_ports; _ } ->
        ( Array.map write_ports ~f:(fun wr -> wr.write_clock) |> Array.to_list
        , Signal.empty )
      | _ -> failwith "unexpected")
  in
  let unique_names l =
    Set.to_list
      (List.fold
         l
         ~init:(Set.empty (module String))
         ~f:(fun set s ->
           try Set.add set (List.hd_exn (Signal.names s)) with
           | _ -> set))
  in
  ( unique_names (List.map clocks_and_resets ~f:fst |> List.concat)
  , unique_names (List.map clocks_and_resets ~f:snd) )
;;

let load_sim vvp_file =
  let command = "`opam config var bin`/hardcaml_vvp.sh " ^ vvp_file in
  ignore (Unix.open_process_out command : Out_channel.t);
  ()
;;

let compile_and_load_sim ?dump_file circuit =
  let verilog_file_name = Filename.temp_file "hardcaml_cosim_" "_verilog" in
  let vvp_file_name = Filename.temp_file "hardcaml_cosim_" "_vvp" in
  at_exit (fun _ ->
    Unix.unlink verilog_file_name;
    Unix.unlink vvp_file_name);
  (* write RTL and testbench *)
  let verilog_file = Out_channel.create verilog_file_name in
  Rtl.output Verilog ~output_mode:(To_channel verilog_file) circuit;
  write_testbench_from_circuit
    ?dump_file
    (Out_channel.output_string verilog_file)
    circuit;
  Out_channel.close verilog_file;
  (* compile *)
  compile [ verilog_file_name ] vvp_file_name;
  (* load simulation *)
  load_sim vvp_file_name
;;

let is_legal_char = function
  | '1' | '0' -> true
  | _ -> false
;;

let rec is_legal s i =
  try if is_legal_char s.[i] then is_legal s (i + 1) else false with
  | _ -> true
;;

let legalise_value s =
  if is_legal s 0
  then s
  else String.map s ~f:(fun c -> if is_legal_char c then c else '0')
;;

let init_sim start_sim inputs outputs =
  (* create server *)
  let server = Comms.create_server net_addr net_port in
  at_exit (fun _ -> Unix.close server);
  (* start simulator *)
  start_sim ();
  (* wait for connection *)
  let server = Comms.accept_client server in
  (* say hello *)
  Comms.recv_string_is server "hello hardcaml";
  ignore
    (Comms.send server (Marshal.to_string (List.map (inputs @ outputs) ~f:fst) []) : int);
  (* set all input ports to zero *)
  ignore
    (control
       server
       (Run
          { sets = List.map inputs ~f:(fun (n, w) -> n, Bits.to_bstr (Bits.zero w))
          ; gets = []
          ; delta_time = 0L
          })
     : response_message);
  server
;;

let make_sim_obj ~server ~clocks ~resets ~inputs ~outputs =
  let inputs = List.map inputs ~f:(fun (n, b) -> n, ref (Bits.zero b)) in
  let outputs = List.map outputs ~f:(fun (n, b) -> n, ref (Bits.zero b)) in
  (* clock cycle update *)
  let clocks_1 = List.map clocks ~f:(fun (n, _) -> n, "1") in
  let clocks_0 = List.map clocks ~f:(fun (n, _) -> n, "0") in
  let get_outputs = List.map outputs ~f:(fun (n, _) -> n) in
  let fcycle () =
    let set_inputs = List.map inputs ~f:(fun (n, v) -> n, Bits.to_bstr !v) in
    ignore
      (control server (Run { sets = clocks_1; gets = []; delta_time = 0L })
       : response_message);
    ignore
      (control server (Run { sets = set_inputs; gets = []; delta_time = 5L })
       : response_message);
    let res =
      control server (Run { sets = clocks_0; gets = get_outputs; delta_time = 5L })
    in
    List.iter2_exn outputs res ~f:(fun (n, v) (n', v') ->
      assert (String.equal n n');
      v := Bits.of_string (legalise_value v'))
  in
  (* reset update *)
  let resets_1 = List.map resets ~f:(fun (n, _) -> n, "1") in
  let resets_0 = List.map resets ~f:(fun (n, _) -> n, "0") in
  let freset () =
    ignore
      (control server (Run { sets = resets_1; gets = []; delta_time = 10L })
       : response_message);
    ignore
      (control server (Run { sets = resets_0; gets = []; delta_time = 0L })
       : response_message)
  in
  (* simulation object *)
  Cyclesim.Private.create
    ~in_ports:inputs
    ~out_ports_before_clock_edge:outputs
    ~out_ports_after_clock_edge:outputs
    ~internal_ports:[]
    ~reset:freset
    ~cycle_check:(fun () -> ())
    ~cycle_before_clock_edge:
      fcycle
    ~cycle_at_clock_edge:(fun () -> ())
    ~cycle_after_clock_edge:(fun () -> ())
    ~lookup_signal:(fun _ -> failwith "sim_lookup_signal not implemented")
    ~lookup_reg:(fun _ -> failwith "sim_lookup_reg not implemented")
;;

(* create simulator from hardcaml circuit *)
let make ?dump_file circuit =
  (* query circuit for ports *)
  let port_name s =
    match Signal.names s with
    | [ n ] -> n
    | _ -> failwith "not a port_name"
  in
  let get_port s = port_name s, Signal.width s in
  let inputs = List.map (Circuit.inputs circuit) ~f:get_port in
  let outputs = List.map (Circuit.outputs circuit) ~f:get_port in
  (* initialize server and simulation *)
  let server =
    init_sim (fun () -> compile_and_load_sim ?dump_file circuit) inputs outputs
  in
  (* create simulation object *)
  let clocks, resets = derive_clocks_and_resets circuit in
  (* remove clocks and resets from input ports *)
  let inputs =
    let cr = clocks @ resets in
    (* inputs without clocks and resets *)
    List.filter inputs ~f:(fun (n, _) -> not (List.mem cr n ~equal:String.equal))
  in
  let clocks, resets =
    List.map clocks ~f:(fun n -> n, 1), List.map resets ~f:(fun n -> n, 1)
  in
  make_sim_obj ~server ~clocks ~resets ~inputs ~outputs
;;

let load ~clocks ~resets ~inputs ~outputs vvp_file =
  (* initialize server and simulation *)
  let server =
    init_sim (fun () -> load_sim vvp_file) (clocks @ resets @ inputs) outputs
  in
  (* create simulation object *)
  make_sim_obj ~server ~clocks ~resets ~inputs ~outputs
;;

module With_interface (I : Interface.S) (O : Interface.S) = struct
  module C = Circuit.With_interface (I) (O)
  module Coerce = Cyclesim.With_interface (I) (O)

  let create =
    Circuit.with_create_options
      (fun create_options
        ?(vcd_file_name = "dump.vcd")
        ?port_checks
        ?add_phantom_inputs
        create_fn
        ->
          let circuit =
            Circuit.call_with_create_options
              C.create_exn
              create_options
              ?port_checks
              ?add_phantom_inputs
              ~name:"cosim"
              create_fn
          in
          let sim = make ~dump_file:vcd_file_name circuit in
          Coerce.coerce sim)
  ;;
end
