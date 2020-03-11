open! Import
include Cosim2_intf

type delta_message =
  { sets : (int * int32 list) list
  ; gets : int list
  ; delta_time : int64
  }

type init_message = string list

type control_message =
  | Finish
  | Run of delta_message

type response_message = (int * int32 list) list

let net_addr = "localhost"
let net_port = 10101

module Comms = struct
  open Unix

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
  let send sock bytes = ignore @@ write sock bytes 0 (Bytes.length bytes)

  let add_int b v =
    for i = 0 to 3 do
      Buffer.add_char b (Char.of_int_exn ((v lsr (i * 8)) land 255))
    done
  ;;

  let send_int =
    let b = Buffer.create 4 in
    fun socket v ->
      Buffer.reset b;
      add_int b v;
      send socket (Buffer.contents_bytes b)
  ;;

  let add_int32 b v =
    for i = 0 to 3 do
      Buffer.add_char
        b
        (Char.of_int_exn Int32.(to_int_exn (logand (shift_right v Int.(i * 8)) 255l)))
    done
  ;;

  let send_int32 =
    let b = Buffer.create 4 in
    fun socket v ->
      Buffer.reset b;
      add_int32 b v;
      send socket (Buffer.contents_bytes b)
  ;;

  let add_int64 b v =
    for i = 0 to 7 do
      Buffer.add_char
        b
        (Char.of_int_exn Int64.(to_int_exn (logand (shift_right v Int.(i * 8)) 255L)))
    done
  ;;

  let send_int64 =
    let b = Buffer.create 8 in
    fun socket v ->
      Buffer.reset b;
      add_int64 b v;
      send socket (Buffer.contents_bytes b)
  ;;

  let send_string socket str =
    send_int socket (String.length str);
    send socket (Bytes.of_string str)
  ;;

  let recv sock bytes len =
    let rec f pos size =
      if size <= 0
      then ()
      else (
        let l = read sock bytes pos size in
        if l <= 0 then failwith "failed to read from socket";
        f (pos + l) (size - l))
    in
    f 0 len
  ;;

  let recv_int32 =
    let b = Bytes.create 4 in
    fun socket ->
      let f i =
        Int32.shift_left (Int32.of_int_exn @@ Char.to_int @@ Bytes.get b i) (i * 8)
      in
      recv socket b 4;
      Int32.(logor (logor (f 0) (f 1)) (logor (f 2) (f 3)))
  ;;

  let recv_int socket = Int32.to_int_exn @@ recv_int32 socket

  let recv_int64 =
    let b = Bytes.create 8 in
    fun socket ->
      let f i =
        Int64.shift_left (Int64.of_int @@ Char.to_int @@ Bytes.get b i) (i * 8)
      in
      recv socket b 4;
      Int64.(
        logor
          (logor (logor (f 0) (f 1)) (logor (f 2) (f 3)))
          (logor (logor (f 4) (f 5)) (logor (f 6) (f 7))))
  ;;

  let recv_string socket =
    let len = recv_int socket in
    (*Printf.printf "recv_string %i\n" len;*)
    let b = Bytes.create len in
    recv socket b len;
    Bytes.to_string b
  ;;

  let recv_string_is socket m =
    let s = recv_string socket in
    if not (String.equal s m)
    then failwith ("failed to get expected string '" ^ m ^ "' got '" ^ s ^ "'")
  ;;
end

(* constants that are used in the hardcaml-vpi c-code *)
let _FINISH = 0
let _RUN = 1
let buf = Buffer.create 1024

let control server message =
  match message with
  | Finish ->
    ignore @@ Comms.send_int server _FINISH;
    []
  | Run { gets; sets; delta_time } ->
    Buffer.reset buf;
    ignore @@ Comms.add_int buf _RUN;
    ignore @@ Comms.add_int64 buf delta_time;
    ignore @@ Comms.add_int buf (List.length sets);
    ignore @@ Comms.add_int buf (List.length gets);
    List.iter sets ~f:(fun (idx, b) ->
      ignore @@ Comms.add_int buf idx;
      List.iter b ~f:(fun b -> ignore @@ Comms.add_int32 buf b));
    List.iter gets ~f:(fun idx -> ignore @@ Comms.add_int buf idx);
    Comms.send server (Buffer.contents_bytes buf);
    List.map gets ~f:(fun idx ->
      let words = Comms.recv_int server in
      let rec f w = if w = 0 then [] else Comms.recv_int32 server :: f (w - 1) in
      idx, f words)
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
     os ("    $dumpfile (\"" ^ dump_file ^ "\");\n");
     os ("    $dumpvars (0, " ^ instance_name name ^ ");\n");
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

module Icarus = struct
  let compile verilog vvp =
    let cmd_line = "iverilog -o " ^ vvp ^ " " ^ String.concat ~sep:" " verilog in
    match Unix.system cmd_line with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith ("Failed to compile verilog to vvp: " ^ cmd_line)
  ;;

  let load_sim ?(opts = "") vvp_file =
    let command =
      "LD_LIBRARY_PATH=$LD_LIBRARY_PATH:`ocamlc -where` vvp "
      ^ "-M`opam config var hardcaml-vpi:lib` "
      ^ "-mhc_ivl "
      ^ opts
      ^ " "
      ^ vvp_file
    in
    ignore (Unix.open_process_out command : Out_channel.t);
    ()
  ;;

  let compile_and_load_sim ?dump_file ?opts circuit =
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
    load_sim ?opts vvp_file_name
  ;;
end

module Modelsim (V : sig
    val vpi : string
  end) =
struct
  (* we'll assume vlib work has been performed already *)
  let compile verilog _ =
    match Unix.system ("vlog " ^ String.concat ~sep:" " verilog) with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith "Failed to compile verilog with modelsim"
  ;;

  let load_sim ?(opts = "-c -do \"run -a\"") tb =
    let command =
      "vsim -pli `opam config var hardcaml-vpi:lib`/" ^ V.vpi ^ " " ^ opts ^ " " ^ tb
    in
    ignore (Unix.open_process_out command : Out_channel.t);
    ()
  ;;

  let compile_and_load_sim ?dump_file ?opts circuit =
    let verilog_file_name = Filename.temp_file "hardcaml_cosim_" "_verilog" in
    at_exit (fun _ -> Unix.unlink verilog_file_name);
    (* write RTL and testbench *)
    let verilog_file = Out_channel.create verilog_file_name in
    Rtl.output Verilog ~output_mode:(To_channel verilog_file) circuit;
    write_testbench_from_circuit
      ?dump_file
      (Out_channel.output_string verilog_file)
      circuit;
    Out_channel.close verilog_file;
    (* compile *)
    compile [ verilog_file_name ] "";
    (* load simulation *)
    let name = Circuit.name circuit ^ "_hardcaml_testbench" in
    load_sim ?opts name
  ;;
end

module Mti32 = Modelsim (struct
    let vpi = "hc_mti.vpi"
  end)

module Mti64 = Modelsim (struct
    let vpi = "hc_mti64.vpi"
  end)

module Make (SIM : Simulator) = struct
  let to_i32l x =
    let rec f x =
      let w = Bits.width x in
      if w <= 32
      then [ Bits.to_int32 x ]
      else Bits.to_int32 (Bits.select x 31 0) :: f (Bits.select x (w - 1) 32)
    in
    f x
  ;;

  let of_i32l w x =
    let rec f w = function
      | [] -> failwith "of_i32l"
      | [ h ] ->
        assert (w <= 32);
        [ Bits.of_int32 ~width:w h ]
      | h :: t ->
        assert (w > 32);
        Bits.of_int32 ~width:32 h :: f (w - 32) t
    in
    Bits.concat_msb (f w x)
  ;;

  let rec read_nets server =
    let idx = Comms.recv_int server in
    if idx < 0
    then []
    else (
      let name = Comms.recv_string server in
      let width = Comms.recv_int server in
      (name, (idx, width)) :: read_nets server)
  ;;

  let init_sim start_sim inputs =
    (* create server *)
    (*Printf.printf "creating server...\n%!";*)
    let server = Comms.create_server net_addr net_port in
    at_exit (fun _ -> Unix.close server);
    (* start simulator *)
    start_sim ();
    (* wait for connection *)
    (*Printf.printf "accept...\n%!";*)
    let server = Comms.accept_client server in
    (* say hello *)
    (*Printf.printf "hello...\n%!";*)
    Comms.recv_string_is server "hello hardcaml";
    (*Printf.printf "got hello...\n%!";*)
    Comms.send_string server "hello verilog";
    let nets = read_nets server in
    (* set all input ports to zero *)
    let sets =
      List.map inputs ~f:(fun (name, w) ->
        let idx, w' = List.Assoc.find_exn nets name ~equal:String.equal in
        assert (w = w');
        idx, to_i32l (Bits.zero w))
    in
    ignore (control server (Run { sets; gets = []; delta_time = 0L }) : response_message);
    server, nets
  ;;

  let make_sim_obj ~server ~clocks ~resets ~inputs ~outputs ~nets =
    let find n = fst (List.Assoc.find_exn nets n ~equal:String.equal) in
    let inputs = List.map inputs ~f:(fun (n, b) -> n, find n, ref (Bits.zero b)) in
    let outputs = List.map outputs ~f:(fun (n, b) -> n, find n, b, ref (Bits.zero b)) in
    (* clock cycle update *)
    let clocks_1 = List.map clocks ~f:(fun (n, _) -> find n, to_i32l Bits.vdd) in
    let clocks_0 = List.map clocks ~f:(fun (n, _) -> find n, to_i32l Bits.gnd) in
    let get_outputs = List.map outputs ~f:(fun (_, n, _, _) -> n) in
    (* {[
         let timer = ref (Unix.gettimeofday ()) in
         let upd_timer () =
           let time = Unix.gettimeofday () in
           let diff = time -. !timer in
           timer := time;
           diff
         in ]} *)
    let fcycle () =
      let set_inputs = List.map inputs ~f:(fun (_, n, v) -> n, to_i32l !v) in
      ignore
        (control server (Run { sets = clocks_1; gets = []; delta_time = 0L })
         : response_message);
      (*let t0 = upd_timer () in*)
      ignore
        (control server (Run { sets = set_inputs; gets = []; delta_time = 5L })
         : response_message);
      (*let t1 = upd_timer () in*)
      let res =
        control server (Run { sets = clocks_0; gets = get_outputs; delta_time = 5L })
      in
      List.iter2_exn outputs res ~f:(fun (_, n, b, v) (n', v') ->
        assert (n = n');
        v := of_i32l b v')
      (* {[
           let t2 = upd_timer () in
           Printf.printf "%6f %6f %6f\n%!" t0 t1 t2;
         ]} *)
    in
    (* reset update *)
    let resets_1 = List.map resets ~f:(fun (n, _) -> find n, to_i32l Bits.vdd) in
    let resets_0 = List.map resets ~f:(fun (n, _) -> find n, to_i32l Bits.gnd) in
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
      ~in_ports:(List.map inputs ~f:(fun (n, _, v) -> n, v))
      ~out_ports_before_clock_edge:(List.map outputs ~f:(fun (n, _, _, v) -> n, v))
      ~out_ports_after_clock_edge:(List.map outputs ~f:(fun (n, _, _, v) -> n, v))
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
  let make ?dump_file ?opts circuit =
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
    let server, nets =
      init_sim (fun () -> SIM.compile_and_load_sim ?dump_file ?opts circuit) inputs
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
    make_sim_obj ~server ~clocks ~resets ~inputs ~outputs ~nets
  ;;

  let load ?opts ~clocks ~resets ~inputs ~outputs vvp_file =
    (* initialize server and simulation *)
    let server, nets =
      init_sim (fun () -> SIM.load_sim ?opts vvp_file) (clocks @ resets @ inputs)
    in
    (* create simulation object *)
    make_sim_obj ~server ~clocks ~resets ~inputs ~outputs ~nets
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
                ~name:"cosim2"
                create_fn
            in
            let sim = make ~dump_file:vcd_file_name circuit in
            Coerce.coerce sim)
    ;;
  end
end
