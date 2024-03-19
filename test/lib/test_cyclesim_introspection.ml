open! Import
open! Signal

let ( <--. ) dst src = dst := Bits.of_int ~width:(Bits.width !dst) src

let assign_reg dst src =
  Cyclesim.Reg.of_bits dst (Bits.of_int ~width:(Cyclesim.Reg.width_in_bits dst) src)
;;

let%expect_test "Cyclesim.internal_port can peek at combinational node" =
  let create_sim () =
    let acc, a, b = input "acc" 8, input "a" 8, input "b" 8 in
    let foo = uresize (a *: b) 8 -- "foo" in
    let summed = acc +: foo -- "summed" in
    let c = output "c" summed in
    Circuit.create_exn ~name:"sim" [ c ]
    |> Cyclesim.create ~config:Cyclesim.Config.trace_all
  in
  let sim = create_sim () in
  let acc = Cyclesim.in_port sim "acc" in
  let a = Cyclesim.in_port sim "a" in
  let b = Cyclesim.in_port sim "b" in
  acc <--. 10;
  a <--. 20;
  b <--. 3;
  Cyclesim.cycle sim;
  let summed =
    Cyclesim.lookup_node_by_name sim "summed" |> Option.value_exn |> Cyclesim.Node.to_int
  in
  let foo =
    Cyclesim.lookup_node_by_name sim "foo" |> Option.value_exn |> Cyclesim.Node.to_int
  in
  Stdio.print_endline [%string "Foo = %{foo#Int}"];
  Stdio.print_endline [%string "Summed = %{summed#Int}"];
  [%expect {|
    Foo = 60
    Summed = 70
    |}]
;;

let%expect_test "lookup_reg can peek and poke internal registers" =
  let create_sim () =
    let clock = input "clock" 1 in
    let x = input "x" 8 in
    let x1 = Signal.reg (Reg_spec.create ~clock ()) x -- "x1" in
    let x2 = Signal.reg (Reg_spec.create ~clock ()) x1 -- "x2" in
    Circuit.create_exn ~name:"sim" [ output "y" x2 ]
    |> Cyclesim.create ~config:Cyclesim.Config.trace_all
  in
  let sim = create_sim () in
  let x = Cyclesim.in_port sim "x" in
  let x1 = Option.value_exn (Cyclesim.lookup_reg_by_name sim "x1") in
  let x2 = Option.value_exn (Cyclesim.lookup_reg_by_name sim "x2") in
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    Option.value_exn
      ~message:"Cannot lookup reg as memory"
      (Cyclesim.lookup_mem_by_name sim "x1"));
  [%expect {| "Cannot lookup reg as memory" |}];
  let print_internal () =
    printf "x1 = %d, x2 = %d\n" (Cyclesim.Reg.to_int x1) (Cyclesim.Reg.to_int x2)
  in
  x <--. 23;
  print_internal ();
  [%expect {| x1 = 0, x2 = 0 |}];
  Cyclesim.cycle sim;
  x <--. 0;
  print_internal ();
  [%expect {| x1 = 23, x2 = 0 |}];
  Cyclesim.cycle sim;
  print_internal ();
  [%expect {| x1 = 0, x2 = 23 |}];
  Cyclesim.cycle sim;
  print_internal ();
  [%expect {| x1 = 0, x2 = 0 |}];
  (* Replace the contents of a register with some dummy value and make sure
     it is picked up by the simulator.
  *)
  assign_reg x1 75;
  Cyclesim.cycle sim;
  print_internal ();
  [%expect {| x1 = 0, x2 = 75 |}]
;;

let assign_mem dst ~address src =
  Cyclesim.Memory.of_bits
    dst
    ~address
    (Bits.of_int ~width:(Cyclesim.Memory.width_in_bits dst) src)
;;

let%expect_test "lookup_mem can read and write internal memory" =
  let create_sim () =
    let read_address = input "read_address" 8 in
    let write_clock = input "clock" 1 in
    let write_address = input "write_address" 8 in
    let write_data = input "write_data" 8 in
    let write_enable = input "write_enable" 1 in
    let mem =
      Signal.multiport_memory
        256
        ~name:"bar"
        ~write_ports:[| { write_clock; write_data; write_enable; write_address } |]
        ~read_addresses:[| read_address |]
    in
    Circuit.create_exn ~name:"memory_circuit" [ output "read_data" mem.(0) ]
    |> Cyclesim.create ~config:Cyclesim.Config.trace_all
  in
  let sim = create_sim () in
  let read_address = Cyclesim.in_port sim "read_address" in
  let write_address = Cyclesim.in_port sim "write_address" in
  let write_data = Cyclesim.in_port sim "write_data" in
  let write_enable = Cyclesim.in_port sim "write_enable" in
  let read_data = Cyclesim.out_port sim "read_data" in
  (* Cannot lookup a memory as a reg. *)
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    Option.value_exn
      ~message:"Cannot lookup memory as reg"
      (Cyclesim.lookup_reg_by_name sim "bar"));
  [%expect {| "Cannot lookup memory as reg" |}];
  let mem = Option.value_exn (Cyclesim.lookup_mem_by_name sim "bar") in
  (* Write mem.(17) normally, and make sure we can read it through [mem] *)
  write_address <--. 17;
  write_data <--. 19;
  write_enable <--. 1;
  Cyclesim.cycle sim;
  printf "peeking mem[17] value = %d" (Cyclesim.Memory.to_int ~address:17 mem);
  [%expect {| peeking mem[17] value = 19 |}];
  write_enable <--. 0;
  (* Poke mem.(42) with a value, and make sure the regular simulation reads out
     that value in the memory.
  *)
  assign_mem mem ~address:42 123;
  read_address <--. 42;
  Cyclesim.cycle sim;
  printf "Read_data = %d" (Bits.to_int !read_data);
  [%expect {| Read_data = 123 |}];
  (* Write a value via hardcaml, and make sure that the value is written after
     calling Cyclesim.cycle
  *)
  assign_mem mem ~address:24 49;
  printf "mem[24] directly after poke = %d" (Cyclesim.Memory.to_int mem ~address:24);
  [%expect {| mem[24] directly after poke = 49 |}];
  write_address <--. 24;
  write_data <--. 64;
  write_enable <--. 1;
  Cyclesim.cycle_before_clock_edge sim;
  printf "mem[24] before clock edge = %d" (Cyclesim.Memory.to_int mem ~address:24);
  [%expect {| mem[24] before clock edge = 49 |}];
  Cyclesim.cycle sim;
  printf "mem[24] after a full cycle = %d" (Cyclesim.Memory.to_int mem ~address:24);
  [%expect {| mem[24] after a full cycle = 64 |}]
;;
