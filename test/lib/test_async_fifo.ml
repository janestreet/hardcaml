open Import
open Hardcaml_waveterm_kernel
open Hardcaml_waveterm_cyclesim

let%expect_test "binary_to_gray" =
  let open Bits in
  let width = 4 in
  for i = 0 to (1 lsl width) - 1 do
    let x = of_unsigned_int ~width i in
    let g = binary_to_gray x in
    Stdio.printf "%s -> %s\n" (to_string g) (to_string (gray_increment g ~by:1))
  done;
  [%expect
    {|
    0000 -> 0001
    0001 -> 0011
    0011 -> 0010
    0010 -> 0110
    0110 -> 0111
    0111 -> 0101
    0101 -> 0100
    0100 -> 1100
    1100 -> 1101
    1101 -> 1111
    1111 -> 1110
    1110 -> 1010
    1010 -> 1011
    1011 -> 1001
    1001 -> 1000
    1000 -> 0000
    |}]
;;

let%expect_test "2 increments by 1, is the same as 1 increment by 2" =
  let open Bits in
  let width = 4 in
  for i = 0 to (1 lsl width) - 1 do
    let x = of_unsigned_int ~width i in
    let g1 = gray_increment ~by:1 (gray_increment ~by:1 x) in
    let g2 = gray_increment ~by:2 x in
    if not (equal g1 g2) then raise_s [%message (x : t) (g1 : t) (g2 : t)]
  done
;;

let%expect_test "gray_inc_mux only changes the input value by 1 bit" =
  let sizes_to_test = [ 1; 4; 8; 9; 12; 13; 17 ] in
  List.iter sizes_to_test ~f:(fun size ->
    let mux =
      Array.of_list (Async_fifo.For_testing.gray_inc_mux_inputs (module Bits) size ~by:1)
    in
    for i = 0 to (1 lsl size) - 1 do
      let gray_inc = mux.(i) in
      let i = Bits.of_unsigned_int ~width:size i in
      let diff = Bits.( ^: ) gray_inc i in
      let popcount = Int.popcount (Bits.to_int_trunc diff) in
      if Int.equal popcount 1
      then ()
      else
        raise_s
          [%message
            "invalid gray code inc mux"
              (size : int)
              (i : Bits.t)
              (gray_inc : Bits.t)
              (popcount : int)]
    done)
;;

let%expect_test "gray_inc_mux only wraps around after 2^size increments" =
  let sizes_to_test = [ 1; 4; 8; 9; 12; 13; 17 ] in
  List.iter sizes_to_test ~f:(fun size ->
    let mux =
      Array.of_list (Async_fifo.For_testing.gray_inc_mux_inputs (module Bits) size ~by:1)
    in
    let expected_num_incs = 1 lsl size in
    let zero = Bits.zero size in
    let rec go_to_next i num_incs =
      if num_incs > expected_num_incs
      then raise_s [%message "went over the expected number of increments"];
      let next = mux.(Bits.to_int_trunc i) in
      let num_incs = num_incs + 1 in
      if Bits.equal next zero then num_incs else go_to_next next num_incs
    in
    let num_incs = go_to_next zero 0 in
    if Int.equal num_incs expected_num_incs
    then ()
    else
      raise_s
        [%message
          "number of increments did not match expected"
            (size : int)
            (num_incs : int)
            (expected_num_incs : int)])
;;

module Async_fifo = Async_fifo.Make (struct
    let width = 72
    let log2_depth = 4
    let optimize_for_same_clock_rate_and_always_reading = false
  end)

module I = Async_fifo.I
module O = Async_fifo.O

let create_sim ?sync_stages () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim =
    Sim.create
      (Async_fifo.create ?sync_stages ~scope:(Scope.create ~flatten_design:true ()))
  in
  Waveform.create sim
;;

let basic_test ?sync_stages () =
  let waves, sim = create_sim ?sync_stages () in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let model = Queue.create () in
  for i = 1 to 3 do
    inputs.data_in := Bits.of_int_trunc i ~width:72;
    inputs.write_enable := Bits.vdd;
    Queue.enqueue model i;
    Cyclesim.cycle sim
  done;
  inputs.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  inputs.read_enable := Bits.vdd;
  for _ = 1 to 4 do
    (match Queue.dequeue model with
     | None -> [%test_result: bool] (Bits.to_bool !(outputs.valid)) ~expect:false
     | Some v ->
       [%test_result: bool] (Bits.to_bool !(outputs.valid)) ~expect:true;
       [%test_result: int] (Bits.to_int_trunc !(outputs.data_out)) ~expect:v);
    Cyclesim.cycle sim
  done;
  inputs.read_enable := Bits.gnd;
  for i = 1 to 16 do
    inputs.data_in := Bits.of_int_trunc i ~width:72;
    inputs.write_enable := Bits.vdd;
    Cyclesim.cycle sim
  done;
  inputs.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  let display_rules =
    Display_rule.
      [ port_name_is "clk" ~wave_format:Bit
      ; port_name_is ~alignment:Right "data_in" ~wave_format:Hex
      ; port_name_is ~alignment:Right "read_enable" ~wave_format:Bit
      ; port_name_is ~alignment:Right "write_enable" ~wave_format:Bit
      ; port_name_is ~alignment:Right "almost_empty" ~wave_format:Bit
      ; port_name_is ~alignment:Right "data_out" ~wave_format:Hex
      ; port_name_is ~alignment:Right "full" ~wave_format:Bit
      ; port_name_is ~alignment:Right "valid" ~wave_format:Bit
      ]
  in
  Waveform.print waves ~display_rules ~wave_width:1 ~display_width:130
;;

let%expect_test "works with a synchronous clock" =
  basic_test ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───────────────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────    │
    │data_in           ││ .01│.02│000000000000000003         │.01│.02│.03│.04│.05│.06│.07│.08│.09│.0A│.0B│.0C│.0D│.0E│.0F│.000010    │
    │                  ││────┴───┴───────────────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────    │
    │read_enable       ││                    ┌───────────────┐                                                                       │
    │                  ││────────────────────┘               └───────────────────────────────────────────────────────────────────    │
    │write_enable      ││────────────┐                       ┌───────────────────────────────────────────────────────────────┐       │
    │                  ││            └───────────────────────┘                                                               └───    │
    │almost_empty      ││────────────────────┐   ┌───────────────────────────────┐                                                   │
    │                  ││                    └───┘                               └───────────────────────────────────────────────    │
    │                  ││────────┬───────────────┬───┬───┬───────────┬───────────────────────────────────────────────────────────    │
    │data_out          ││ .000000│.00000000000001│.02│.03│.0000000000│000000000000000001                                             │
    │                  ││────────┴───────────────┴───┴───┴───────────┴───────────────────────────────────────────────────────────    │
    │full              ││                                                                                                ┌───────    │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────┘           │
    │valid             ││            ┌───────────────────┐               ┌───────────────────────────────────────────────────────    │
    │                  ││────────────┘                   └───────────────┘                                                           │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  basic_test ~sync_stages:3 ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───────────────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────    │
    │data_in           ││ .01│.02│000000000000000003         │.01│.02│.03│.04│.05│.06│.07│.08│.09│.0A│.0B│.0C│.0D│.0E│.0F│.000010    │
    │                  ││────┴───┴───────────────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────    │
    │read_enable       ││                    ┌───────────────┐                                                                       │
    │                  ││────────────────────┘               └───────────────────────────────────────────────────────────────────    │
    │write_enable      ││────────────┐                       ┌───────────────────────────────────────────────────────────────┐       │
    │                  ││            └───────────────────────┘                                                               └───    │
    │almost_empty      ││────────────────────────────────────────────────────────────┐                                               │
    │                  ││                                                            └───────────────────────────────────────────    │
    │                  ││────────┬───────────────┬───┬───┬───────────┬───────────────────────────────────────────────────────────    │
    │data_out          ││ .000000│.00000000000001│.02│.03│.0000000000│000000000000000001                                             │
    │                  ││────────┴───────────────┴───┴───┴───────────┴───────────────────────────────────────────────────────────    │
    │full              ││                                                                                                ┌───────    │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────┘           │
    │valid             ││                ┌───────────────┐                   ┌───────────────────────────────────────────────────    │
    │                  ││────────────────┘               └───────────────────┘                                                       │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}];
  basic_test ~sync_stages:4 ();
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───────────────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────    │
    │data_in           ││ .01│.02│000000000000000003         │.01│.02│.03│.04│.05│.06│.07│.08│.09│.0A│.0B│.0C│.0D│.0E│.0F│.000010    │
    │                  ││────┴───┴───────────────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────    │
    │read_enable       ││                    ┌───────────────┐                                                                       │
    │                  ││────────────────────┘               └───────────────────────────────────────────────────────────────────    │
    │write_enable      ││────────────┐                       ┌───────────────────────────────────────────────────────────────┐       │
    │                  ││            └───────────────────────┘                                                               └───    │
    │almost_empty      ││────────────────────────────────────────────────────────────────┐                                           │
    │                  ││                                                                └───────────────────────────────────────    │
    │                  ││────────┬───────────────┬───┬───┬───────────┬───────────────────────────────────────────────────────────    │
    │data_out          ││ .000000│.00000000000001│.02│.03│.0000000000│000000000000000001                                             │
    │                  ││────────┴───────────────┴───┴───┴───────────┴───────────────────────────────────────────────────────────    │
    │full              ││                                                                                                ┌───────    │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────┘           │
    │valid             ││                    ┌───────────┐                       ┌───────────────────────────────────────────────    │
    │                  ││────────────────────┘           └───────────────────────┘                                                   │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let create_sim_delay () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim =
    Sim.create
      (Async_fifo.create_with_delay ~delay:3 (Scope.create ~flatten_design:true ()))
  in
  Waveform.create sim
;;

let%expect_test "works with a synchronous clock in delayed mode" =
  let waves, sim = create_sim_delay () in
  let inputs = Cyclesim.inputs sim in
  for i = 1 to 3 do
    inputs.data_in := Bits.of_int_trunc i ~width:72;
    inputs.write_enable := Bits.vdd;
    Cyclesim.cycle sim
  done;
  inputs.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  Cyclesim.cycle sim;
  inputs.read_enable := Bits.vdd;
  for _ = 1 to 4 do
    Cyclesim.cycle sim
  done;
  inputs.read_enable := Bits.gnd;
  for i = 1 to 16 do
    inputs.data_in := Bits.of_int_trunc i ~width:72;
    inputs.write_enable := Bits.vdd;
    Cyclesim.cycle sim
  done;
  inputs.write_enable := Bits.gnd;
  Cyclesim.cycle sim;
  let display_rules =
    Display_rule.
      [ port_name_is "clk" ~wave_format:Bit
      ; port_name_is ~alignment:Right "data_in" ~wave_format:Hex
      ; port_name_is ~alignment:Right "read_enable" ~wave_format:Bit
      ; port_name_is ~alignment:Right "write_enable" ~wave_format:Bit
      ; port_name_is ~alignment:Right "almost_empty" ~wave_format:Bit
      ; port_name_is ~alignment:Right "data_out" ~wave_format:Hex
      ; port_name_is ~alignment:Right "full" ~wave_format:Bit
      ; port_name_is ~alignment:Right "valid" ~wave_format:Bit
      ]
  in
  Waveform.print waves ~display_rules ~wave_width:1 ~display_width:130;
  [%expect
    {|
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │                  ││────┬───┬───────────────────────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───────    │
    │data_in           ││ .01│.02│000000000000000003         │.01│.02│.03│.04│.05│.06│.07│.08│.09│.0A│.0B│.0C│.0D│.0E│.0F│.000010    │
    │                  ││────┴───┴───────────────────────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───────    │
    │read_enable       ││                    ┌───────────────┐                                                                       │
    │                  ││────────────────────┘               └───────────────────────────────────────────────────────────────────    │
    │write_enable      ││────────────┐                       ┌───────────────────────────────────────────────────────────────┐       │
    │                  ││            └───────────────────────┘                                                               └───    │
    │almost_empty      ││────────────────────┐       ┌───────────────────────────┐                                                   │
    │                  ││                    └───────┘                           └───────────────────────────────────────────────    │
    │                  ││────────┬───────────────────┬───┬───┬───────┬───────────────────────────────────────────────────────────    │
    │data_out          ││ .000000│000000000000000001 │.02│.03│.000000│000000000000000001                                             │
    │                  ││────────┴───────────────────┴───┴───┴───────┴───────────────────────────────────────────────────────────    │
    │full              ││                                                                                                ┌───────    │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────┘           │
    │valid             ││                        ┌───────────┐                       ┌───────────────────────────────────────────    │
    │                  ││────────────────────────┘           └───────────────────────┘                                               │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
