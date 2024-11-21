(* This tests the Async FIFO module in Hardcaml. *)

open Core
open Hardcaml

module Fifo = Async_fifo.Make (struct
    (* 8 deep, 4 wide *)
    let width = 4
    let log2_depth = 3
  end)

include struct
  open Hardcaml_event_driven_sim
  module Logic = Two_state_logic
  module Evsim = Make (Logic)
  module Config = Config
  module Waveform = Waveterm.Waveform
end

include struct
  open Event_driven_sim
  module Simulator = Simulator
  module Process = Simulator.Process

  let ( <-- ) = Simulator.( <-- )
  let ( !& ) = Simulator.( !& )
  let ( !! ) = Simulator.( !! )
end

module Fifo_sim = Evsim.With_interface (Fifo.I) (Fifo.O)

let fifo () =
  Fifo.create
    ~scope:(Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
;;

let%expect_test "show expected behavior of the async fifo (and show that the cyclesim \
                 based event simulator gives the same output)"
  =
  List.iter Bool.all ~f:(fun use_cyclesim ->
    let waves, { Fifo_sim.simulator; _ } =
      Fifo_sim.with_waveterm
        ~config:{ Config.default with use_cyclesim }
        (fifo ())
        (fun inputs _outputs ->
           [ Fifo_sim.create_clock inputs.clock_read.signal ~time:3
           ; Fifo_sim.create_clock inputs.clock_write.signal ~time:5
           ; Process.create
               [ !&(inputs.clock_write.signal) ]
               (let cnt = ref 0 in
                fun () ->
                  if Logic.is_vdd !!(inputs.clock_write.signal)
                  then (
                    Int.incr cnt;
                    let write_data_if ~cycle_is ~data =
                      if !cnt = cycle_is
                      then (
                        inputs.data_in.signal <-- Logic.of_int ~width:4 data;
                        inputs.write_enable.signal <-- Logic.vdd)
                    in
                    inputs.write_enable.signal <-- Logic.gnd;
                    write_data_if ~cycle_is:2 ~data:10;
                    write_data_if ~cycle_is:4 ~data:11))
           ; Process.create
               [ !&(inputs.clock_read.signal) ]
               (let cnt = ref 0 in
                fun () ->
                  if Logic.is_vdd !!(inputs.clock_read.signal)
                  then (
                    Int.incr cnt;
                    if !cnt = 7
                    then inputs.read_enable.signal <-- Logic.vdd
                    else inputs.read_enable.signal <-- Logic.gnd))
           ])
    in
    Simulator.run ~time_limit:100 simulator;
    Waveform.expect waves ~wave_width:(-1) ~display_width:82 ~display_height:30;
    [%expect
      {|
      ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────┐
      │almost_empty      ││────────────────────────────────────────────────────────────│
      │                  ││                                                            │
      │clock_read        ││   ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
      │                  ││───┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
      │clock_write       ││     ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────│
      │                  ││─────┘    └────┘    └────┘    └────┘    └────┘    └────┘    │
      │                  ││───────────────┬───────────────────┬────────────────────────│
      │data_in           ││ 0             │A                  │B                       │
      │                  ││───────────────┴───────────────────┴────────────────────────│
      │                  ││───────────────────────────┬─────────────────┬─────┬────────│
      │data_out          ││ 0                         │A                │0    │B       │
      │                  ││───────────────────────────┴─────────────────┴─────┴────────│
      │full              ││                                                            │
      │                  ││────────────────────────────────────────────────────────────│
      │read_enable       ││                                       ┌─────┐              │
      │                  ││───────────────────────────────────────┘     └──────────────│
      │reset_read        ││                                                            │
      │                  ││────────────────────────────────────────────────────────────│
      │reset_write       ││                                                            │
      │                  ││────────────────────────────────────────────────────────────│
      │valid             ││                                 ┌───────────┐           ┌──│
      │                  ││─────────────────────────────────┘           └───────────┘  │
      │write_enable      ││               ┌─────────┐         ┌─────────┐              │
      │                  ││───────────────┘         └─────────┘         └──────────────│
      │                  ││                                                            │
      │                  ││                                                            │
      │                  ││                                                            │
      │                  ││                                                            │
      └──────────────────┘└────────────────────────────────────────────────────────────┘
      e9fa5d6b717e9173b3ded2a2613d4682
      |}])
;;
