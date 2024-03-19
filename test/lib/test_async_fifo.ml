open Import
open Hardcaml_waveterm_kernel
open Hardcaml_waveterm_cyclesim

module Async_fifo = Async_fifo.Make (struct
  let width = 72
  let log2_depth = 4
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
    inputs.data_in := Bits.of_int i ~width:72;
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
       [%test_result: int] (Bits.to_int !(outputs.data_out)) ~expect:v);
    Cyclesim.cycle sim
  done;
  inputs.read_enable := Bits.gnd;
  for i = 1 to 16 do
    inputs.data_in := Bits.of_int i ~width:72;
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
  Waveform.print waves ~display_rules ~wave_width:1 ~display_width:130 ~display_height:25
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
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
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
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
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
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
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
    inputs.data_in := Bits.of_int i ~width:72;
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
    inputs.data_in := Bits.of_int i ~width:72;
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
  Waveform.print waves ~display_rules ~wave_width:1 ~display_width:130 ~display_height:20;
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
    │                  ││                                                                                                            │
    │                  ││                                                                                                            │
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
