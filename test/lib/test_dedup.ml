open Core
open Hardcaml
open Signal
open Signal.Unoptimized

let print_circuit c =
  Circuit.signal_graph c
  |> Signal_graph.iter ~f:(fun signal -> Signal.to_string signal |> Stdio.print_endline)
;;

let%expect_test "simple" =
  let dedup outputs = Circuit.create_exn ~name:"test" outputs |> Dedup.deduplicate in
  let v = of_string "1111" +: of_string "1111" in
  let c = dedup [ output "out" v ] in
  print_circuit c;
  [%expect
    {|
    Wire[id:1 bits:4 names:out deps:3] -> 3
    Op[id:3 bits:4 names: deps:2,2] = add
    Const[id:2 bits:4 names: deps:] = 1111 |}];
  let v =
    of_string "1111"
    +: of_string "1111"
    +: of_string "1110"
    +: (of_string "1111" +: of_string "1111")
  in
  let c = dedup [ output "out" v ] in
  print_circuit c;
  (* First addition is deduplicated with the last one. Also all constants are deduplicated. *)
  [%expect
    {|
  Wire[id:1 bits:4 names:out deps:6] -> 6
  Op[id:6 bits:4 names: deps:5,3] = add
  Op[id:5 bits:4 names: deps:3,4] = add
  Op[id:3 bits:4 names: deps:2,2] = add
  Const[id:2 bits:4 names: deps:] = 1111
  Const[id:4 bits:4 names: deps:] = 1110 |}]
;;

let%expect_test "register" =
  let dedup outputs = Circuit.create_exn ~name:"test" outputs |> Dedup.deduplicate in
  let clock = input "clock" 1 in
  let v =
    reg_fb
      (Reg_spec.create () ~clock)
      (fun x -> x +: of_string "1111")
      ~enable:(of_string "1")
      ~w:4
  in
  let c = dedup [ output "out" (v +: of_string "1111") ] in
  print_circuit c;
  (* Deduplication of the constant inside the register. Additionally we see that
     deduplication inserts additional wires between registers and combinatorial logic.
     (we could eliminate them in most cases, but let's keep them for simplicity)
  *)
  [%expect
    {|
    Wire[id:4 bits:4 names:out deps:6] -> 6
    Op[id:6 bits:4 names: deps:3,5] = add
    Wire[id:3 bits:4 names: deps:9] -> 9
    Reg[id:9 bits:4 names: deps:2,1,0,7,0,7,8]
    Wire[id:2 bits:4 names: deps:6] -> 6
    Wire[id:1 bits:1 names:clock deps:0] -> 0
    Empty
    Const[id:7 bits:4 names: deps:] = 0000
    Const[id:8 bits:1 names:vdd deps:] = 1
    Const[id:5 bits:4 names: deps:] = 1111 |}]
;;

let%expect_test "wires" =
  let dedup outputs = Circuit.create_exn ~name:"test" outputs |> Dedup.deduplicate in
  let v = of_string "1100" in
  let c = dedup [ output "out" (wireof (wireof (wireof (wireof v) -- "x"))) ] in
  print_circuit c;
  (* Unnamed wires are collapsed. *)
  [%expect
    {|
    Wire[id:2 bits:4 names:out deps:1] -> 1
    Wire[id:1 bits:4 names:x deps:3] -> 3
    Const[id:3 bits:4 names: deps:] = 1100 |}]
;;

let%expect_test "memory" =
  let dedup outputs = Circuit.create_exn ~name:"test" outputs |> Dedup.deduplicate in
  let clock = input "clock" 1 in
  let v =
    multiport_memory
      ~name:"memory1"
      16
      ~write_ports:
        [| { Signal.write_clock = clock
           ; write_address = of_string "1100"
           ; write_enable = of_string "1"
           ; write_data = of_string "1100"
           }
        |]
      ~read_addresses:[| of_string "1011"; of_string "1011" |]
  in
  let read_val_1 = v.(0) in
  let read_val_2 = v.(1) in
  let c = dedup [ output "out" (read_val_1 +: read_val_2 +: of_string "1111") ] in
  (* Deduplication of memory_read_port. Notice that Mem_read_port directly contains
     Multiport_mem, not a wire to it and that Mem_read_port is also deduplicated. *)
  print_circuit c;
  [%expect
    {|
    Wire[id:3 bits:4 names:out deps:11] -> 11
    Op[id:11 bits:4 names: deps:9,10] = add
    Op[id:9 bits:4 names: deps:2,2] = add
    Wire[id:2 bits:4 names: deps:8] -> 8
    Mem_read_port[id:8 bits:4 names: deps:7,4]
    Multiport_mem[id:7 bits:4 names:memory1 deps:4,4,1,5,5,6]
    Const[id:4 bits:4 names: deps:] = 1011
    Wire[id:1 bits:1 names:clock deps:0] -> 0
    Empty
    Const[id:5 bits:4 names: deps:] = 1100
    Const[id:6 bits:1 names: deps:] = 1
    Const[id:10 bits:4 names: deps:] = 1111 |}]
;;

let trials = 500

let%test_unit "registers/memories not eliminated" =
  let num_registers c =
    Circuit.signal_graph c |> Signal_graph.filter ~f:Signal.is_reg |> List.length
  in
  let num_memories c =
    Circuit.signal_graph c
    |> Signal_graph.filter ~f:Signal.is_multiport_mem
    |> List.length
  in
  Quickcheck.test
    ~trials
    (Generator.gen_circuit ~allow_inputs:true ~depth:3)
    ~f:(fun circuit ->
      let circuit1 = Dedup.deduplicate circuit in
      [%test_result: int] (num_registers circuit1) ~expect:(num_registers circuit);
      [%test_result: int] (num_memories circuit1) ~expect:(num_memories circuit))
;;

let cycle_count = 10

let cyclesim_eval inputs circuit =
  let sim = Cyclesim.create circuit in
  Cyclesim.reset sim;
  let results = ref [] in
  let out_port = Cyclesim.out_port sim "out" in
  List.iter inputs ~f:(fun input_values ->
    List.iter input_values ~f:(fun (name, value) -> Cyclesim.in_port sim name := value);
    Cyclesim.cycle sim;
    results := !out_port :: !results);
  !results |> List.rev
;;

let gen_circuit_and_inputs =
  let open Quickcheck.Let_syntax in
  let%bind circuit = Generator.gen_circuit ~allow_inputs:true ~depth:3 in
  let%map inputs =
    Quickcheck.Generator.list_with_length cycle_count (Generator.gen_input_data circuit)
  in
  circuit, inputs
;;

let%test_unit "sufficient portion of generated circuits is not just constant outputs." =
  let random =
    Splittable_random.State.create (Base.Random.State.make [| 0xdeadbeef |])
  in
  let number_of_circuits = 1_000 in
  let results =
    List.init number_of_circuits ~f:(fun _ ->
      let circuit, inputs =
        Base_quickcheck.Generator.generate gen_circuit_and_inputs ~size:1000 ~random
      in
      let bits = cyclesim_eval inputs circuit in
      bits)
  in
  let number_of_entries_that_are_not_just_constant_outputs =
    List.count results ~f:(fun bits ->
      let hd = List.hd_exn bits in
      List.exists bits ~f:(fun arg -> not (Bits.equal hd arg)))
  in
  assert (number_of_entries_that_are_not_just_constant_outputs * 3 > number_of_circuits)
;;

let%test_unit "behaviour is preserved" =
  Quickcheck.test ~trials gen_circuit_and_inputs ~f:(fun (circuit, inputs) ->
    let circuit1 = Dedup.deduplicate circuit in
    let result = cyclesim_eval inputs circuit in
    let result1 = cyclesim_eval inputs circuit1 in
    [%test_result: Bits.t list] result1 ~expect:result)
;;
