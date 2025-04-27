open! Import
open! Hardcaml_waveterm_cyclesim

let write_port address_width data_width enable_width =
  { Write_port.write_clock = Signal.gnd
  ; write_address = Signal.of_int_trunc ~width:address_width 0
  ; write_data = Signal.of_int_trunc ~width:data_width 0
  ; write_enable = Signal.of_int_trunc ~width:enable_width 0
  }
;;

let assert_read_data_widths q expected =
  Array.iter2_exn q expected ~f:(fun q expected ->
    let got = Signal.width q in
    if got <> expected
    then raise_s [%message "Read data width is wrong" (got : int) (expected : int)])
;;

let%expect_test "ratio calculations and exceptions" =
  require_does_not_raise (fun () ->
    assert_read_data_widths
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:false
         16
         ~write_ports:[| write_port 4 8 1; write_port 4 8 2 |]
         ~read_addresses:[| Signal.zero 3; Signal.zero 4 |])
      [| 16; 8 |]);
  [%expect {| |}];
  require_does_not_raise (fun () ->
    assert_read_data_widths
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:false
         16
         ~write_ports:[| write_port 5 8 1; write_port 4 16 4 |]
         ~read_addresses:[| Signal.zero 2 |])
      [| 64 |]);
  [%expect {| |}];
  require_does_not_raise (fun () ->
    assert_read_data_widths
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:false
         32
         ~write_ports:[| write_port 6 6 1; write_port 5 12 2 |]
         ~read_addresses:[| Signal.zero 7 |])
      [| 3 |]);
  [%expect {| |}];
  require_does_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:false
         8
         ~write_ports:[| write_port 4 6 1; write_port 2 12 1 |]
         ~read_addresses:[| Signal.zero 5; Signal.zero 2 |]
       : Signal.t array));
  [%expect
    {|
    ("[Signal.multiport_memory] Width of write port data and address are inconsistent with widest port"
     (write_port_data_width     6)
     (write_port_address_width  4)
     (ratio                     4)
     (widest_port_data_width    12)
     (widest_port_address_width 2))
    |}];
  require_does_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:false
         8
         ~write_ports:[| write_port 4 6 1; write_port 4 12 1 |]
         ~read_addresses:[| Signal.zero 5; Signal.zero 2 |]
       : Signal.t array));
  [%expect
    {|
    ("[Signal.multiport_memory] Width of write port data and address are inconsistent with widest port"
     (write_port_data_width     6)
     (write_port_address_width  4)
     (ratio                     1)
     (widest_port_data_width    12)
     (widest_port_address_width 4))
    |}];
  require_does_raise (fun () ->
    ignore
      (Signal.multiport_memory
         ~enable_modelling_features:true
         ~verbose:false
         16
         ~write_ports:[| write_port 4 6 1 |]
         ~read_addresses:[| Signal.zero 7 |]
       : Signal.t array));
  [%expect {| ("Cannot split read port" (ratio 8)) |}]
;;

type write_bits =
  { address_bits : int
  ; data_bits : int
  ; enable_bits : int
  }

let circ ~initialize_to ~write_bits ~read_bits =
  let open Signal in
  let write_clock = input "write_clock" 1 in
  let min_address_bits =
    Array.fold write_bits ~init:Int.max_value ~f:(fun min { address_bits; _ } ->
      Int.min min address_bits)
  in
  let write_port idx { address_bits; data_bits; enable_bits } =
    let input n b = input (n ^ Int.to_string idx) b in
    { Write_port.write_clock
    ; write_address = input "write_address" address_bits
    ; write_data = input "write_data" data_bits
    ; write_enable = input "write_enable" enable_bits
    }
  in
  let write_ports = Array.mapi write_bits ~f:write_port in
  let read_addresses =
    Array.mapi read_bits ~f:(fun idx address_width ->
      input ("read_address" ^ Int.to_string idx) address_width)
  in
  let q =
    Signal.multiport_memory
      ~enable_modelling_features:true
      ?initialize_to
      (Int.pow 2 min_address_bits)
      ~write_ports
      ~read_addresses
  in
  let output i q = output ("q" ^ Int.to_string i) q in
  Circuit.create_exn ~name:"membe" (Array.mapi q ~f:output |> Array.to_list)
;;

let get_write_port sim idx =
  { Write_port.write_clock = ref Bits.gnd
  ; write_address = Cyclesim.in_port sim ("write_address" ^ Int.to_string idx)
  ; write_data = Cyclesim.in_port sim ("write_data" ^ Int.to_string idx)
  ; write_enable = Cyclesim.in_port sim ("write_enable" ^ Int.to_string idx)
  }
;;

let sim ~initialize_to ~write_bits ~read_bits =
  let open Bits in
  let circ = circ ~initialize_to ~write_bits ~read_bits in
  let sim = Cyclesim.create circ in
  let waves, sim = Waveform.create sim in
  let write_ports = Array.init (Array.length write_bits) ~f:(get_write_port sim) in
  let read_addresses =
    Array.init (Array.length read_bits) ~f:(fun idx ->
      Cyclesim.in_port sim ("read_address" ^ Int.to_string idx))
  in
  let q =
    Array.init (Array.length read_bits) ~f:(fun idx ->
      Cyclesim.out_port sim ("q" ^ Int.to_string idx))
  in
  let write idx address data enable =
    write_ports.(idx).write_address <--. address;
    write_ports.(idx).write_data <--. data;
    write_ports.(idx).write_enable <--. enable
  in
  let cycle () =
    Cyclesim.cycle sim;
    Array.iter write_ports ~f:(fun write_port -> write_port.write_enable <--. 0)
  in
  waves, cycle, write, read_addresses, q
;;

let ram_model ~initialize_to ~write_bits ~read_bits =
  let debug = false in
  let memory = Array.copy initialize_to in
  let base_data_width = Bits.width memory.(0) in
  let min_address_bits =
    Array.fold write_bits ~init:Int.max_value ~f:(fun min { address_bits; _ } ->
      Int.min min address_bits)
  in
  let read port address =
    let diff = read_bits.(port) - min_address_bits in
    if diff = 0
    then memory.(address)
    else if diff > 0
    then (
      let ratio = Int.pow 2 diff in
      let address, word = address / ratio, address % ratio in
      let width = base_data_width / ratio in
      if debug
      then print_s [%message (ratio : int) (address : int) (word : int) (width : int)];
      memory.(address).Bits.:+[width * word, Some width])
    else (
      let ratio = Int.pow 2 (-diff) in
      List.init ratio ~f:(fun i -> memory.((address * ratio) + i)) |> Bits.concat_lsb)
  in
  let write port address data enable =
    let { address_bits = _; data_bits; enable_bits } = write_bits.(port) in
    let ratio = base_data_width / data_bits in
    let address, word = address / ratio, address % ratio in
    let data =
      Bits.of_int_trunc ~width:data_bits data
      |> Bits.uresize ~width:(ratio * data_bits)
      |> Bits.sll ~by:(data_bits * word)
    in
    let enable = Bits.of_int_trunc ~width:enable_bits enable in
    let mask =
      Bits.bits_lsb enable
      |> List.map ~f:(fun b -> Bits.repeat b ~count:(data_bits / enable_bits))
      |> Bits.concat_lsb
      |> Bits.uresize ~width:(ratio * data_bits)
      |> Bits.sll ~by:(data_bits * word)
    in
    if debug
    then
      print_s
        [%message
          (address : int)
            (word : int)
            (ratio : int)
            (data : Bits.Hex.t)
            (enable : Bits.Hex.t)
            (mask : Bits.Hex.t)];
    memory.(address) <- Bits.(memory.(address) &: ~:mask |: (data &: mask))
  in
  read, write
;;

let%expect_test "model" =
  let read, write =
    ram_model
      ~initialize_to:(Array.init 16 ~f:(fun _ -> Bits.zero 32))
      ~write_bits:[| { address_bits = 4; data_bits = 32; enable_bits = 4 } |]
      ~read_bits:[| 3; 4; 5 |]
  in
  write 0 0 0x12345678 0xe;
  write 0 1 0x1abbccdd 0x7;
  print_s [%message (read 0 0 : Bits.Hex.t)];
  [%expect {| ("read 0 0" 64'h00bbccdd12345600) |}];
  print_s [%message (read 1 0 : Bits.Hex.t) (read 1 1 : Bits.Hex.t)];
  [%expect
    {|
    (("read 1 0" 32'h12345600)
     ("read 1 1" 32'h00bbccdd))
    |}];
  print_s
    [%message
      (read 2 0 : Bits.Hex.t)
        (read 2 1 : Bits.Hex.t)
        (read 2 2 : Bits.Hex.t)
        (read 2 3 : Bits.Hex.t)];
  [%expect
    {|
    (("read 2 0" 16'h5600)
     ("read 2 1" 16'h1234)
     ("read 2 2" 16'hccdd)
     ("read 2 3" 16'h00bb))
    |}];
  let read, write =
    ram_model
      ~initialize_to:(Array.init 16 ~f:(fun _ -> Bits.zero 32))
      ~write_bits:
        [| { address_bits = 4; data_bits = 32; enable_bits = 4 }
         ; { address_bits = 5; data_bits = 16; enable_bits = 2 }
        |]
      ~read_bits:[| 4 |]
  in
  write 0 2 0x12345678 0xf;
  print_s [%message (read 0 2 : Bits.Hex.t)];
  [%expect {| ("read 0 2" 32'h12345678) |}];
  write 1 5 0x4321 0x3;
  print_s [%message (read 0 2 : Bits.Hex.t)];
  [%expect {| ("read 0 2" 32'h43215678) |}];
  write 1 4 0xaa 0x1;
  print_s [%message (read 0 2 : Bits.Hex.t)];
  [%expect {| ("read 0 2" 32'h432156aa) |}]
;;

let test_randomly ~write_bits ~read_bits =
  let open Bits in
  let min_address_bits, max_data_bits =
    Array.fold
      write_bits
      ~init:(Int.max_value, 0)
      ~f:(fun (min, max_data_bits) { address_bits; data_bits; _ } ->
        if address_bits < min then address_bits, data_bits else min, max_data_bits)
  in
  let size = Int.pow 2 min_address_bits in
  let initialize_to = Array.init size ~f:(fun _ -> random ~width:max_data_bits) in
  let _, cycle, write_sim, read_addresses, q =
    sim ~initialize_to:(Some initialize_to) ~write_bits ~read_bits
  in
  let read_model, write_model = ram_model ~initialize_to ~write_bits ~read_bits in
  for _ = 0 to 9 do
    (* perform 10 random writes *)
    for _ = 0 to 10 do
      let port = Random.int (Array.length write_bits) in
      let { address_bits; data_bits; enable_bits } = write_bits.(port) in
      let data = Random.int (Int.pow 2 data_bits) in
      let address = Random.int (Int.pow 2 address_bits) in
      let enable = Random.int (Int.pow 2 enable_bits) in
      write_model port address data enable;
      write_sim port address data enable;
      cycle ()
    done;
    Array.iteri read_bits ~f:(fun port read_bits ->
      let size = Int.pow 2 read_bits in
      for address = 0 to size - 1 do
        read_addresses.(port) <--. address;
        cycle ();
        let model = read_model port address in
        if not (equal model !(q.(port)))
        then
          raise_s
            [%message
              "MISMATCH"
                (port : int)
                (read_bits : int)
                (address : int)
                (model : Bits.Hex.t)
                (!(q.(port)) : Bits.Hex.t)]
      done)
  done
;;

let%expect_test "randomly against a model" =
  test_randomly
    ~write_bits:[| { address_bits = 4; data_bits = 8; enable_bits = 1 } |]
    ~read_bits:[| 3; 4; 5 |];
  test_randomly
    ~write_bits:
      [| { address_bits = 4; data_bits = 8; enable_bits = 1 }
       ; { address_bits = 5; data_bits = 4; enable_bits = 2 }
      |]
    ~read_bits:[| 3; 4; 5 |];
  test_randomly
    ~write_bits:
      [| { address_bits = 4; data_bits = 12; enable_bits = 3 }
       ; { address_bits = 5; data_bits = 6; enable_bits = 3 }
      |]
    ~read_bits:[| 2; 4; 6 |]
;;
