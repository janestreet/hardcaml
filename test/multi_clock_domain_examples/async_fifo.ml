(* This tests the Async FIFO module in Hardcaml. *)

open Core
open Hardcaml

module Fifo = Async_fifo.Make (struct
    (* 8 deep, 4 wide *)
    let width = 4
    let log2_depth = 3
    let optimize_for_same_clock_rate_and_always_reading = false
  end)

let fifo () =
  Fifo.create
    ~scope:(Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ())
;;

(* Ev sim tests *)
module _ = struct
  open Hardcaml_event_driven_sim.Two_state_simulator
  module Process = Simulator.Process
  module Waveform = Waveterm.Waveform

  let ( <-- ) = Simulator.( <-- )
  let ( !& ) = Simulator.( !& )
  let ( !! ) = Simulator.( !! )

  module Fifo_sim = With_interface (Fifo.I) (Fifo.O)

  let%expect_test "show expected behavior of the async fifo (and show that the cyclesim \
                   based event simulator gives the same output)"
    =
    List.iter Hardcaml_event_driven_sim.Sim_mode.all ~f:(fun sim_mode ->
      let waves, { Fifo_sim.simulator; _ } =
        Fifo_sim.with_waveterm
          ~config:{ Config.trace_all with sim_mode }
          (fifo ())
          (fun inputs _outputs ->
             [ Fifo_sim.create_clock inputs.clock_read.signal ~time:3
             ; Fifo_sim.create_clock inputs.clock_write.signal ~time:5
             ; Process.create
                 [ !&(inputs.clock_write.signal) ]
                 (let cnt = ref 0 in
                  fun () ->
                    if Logic.to_bool !!(inputs.clock_write.signal)
                    then (
                      Int.incr cnt;
                      let write_data_if ~cycle_is ~data =
                        if !cnt = cycle_is
                        then (
                          inputs.data_in.signal <-- Logic.of_int_trunc ~width:4 data;
                          inputs.write_enable.signal <-- Logic.vdd)
                      in
                      inputs.write_enable.signal <-- Logic.gnd;
                      write_data_if ~cycle_is:2 ~data:10;
                      write_data_if ~cycle_is:4 ~data:11))
             ; Process.create
                 [ !&(inputs.clock_read.signal) ]
                 (let cnt = ref 0 in
                  fun () ->
                    if Logic.to_bool !!(inputs.clock_read.signal)
                    then (
                      Int.incr cnt;
                      if !cnt = 7
                      then inputs.read_enable.signal <-- Logic.vdd
                      else inputs.read_enable.signal <-- Logic.gnd))
             ])
      in
      Simulator.run ~time_limit:100 simulator;
      Waveform.expect waves ~wave_width:(-1) ~display_width:82;
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
        │                  ││───────────────────────────┬─────────────────┬─────┬────────│
        │data_out_0        ││ 0                         │A                │0    │B       │
        │                  ││───────────────────────────┴─────────────────┴─────┴────────│
        │full              ││                                                            │
        │                  ││────────────────────────────────────────────────────────────│
        │gnd               ││                                                            │
        │                  ││────────────────────────────────────────────────────────────│
        │                  ││─────────────────────────────────────────────┬──────────────│
        │raddr_rd          ││ 0                                           │1             │
        │                  ││─────────────────────────────────────────────┴──────────────│
        │                  ││────────────────────────────────────────────────────────────│
        │raddr_wd          ││ 0                                                          │
        │                  ││────────────────────────────────────────────────────────────│
        │                  ││───────────────────────────────────────────────────────┬────│
        │raddr_wd_ff_0     ││ 0                                                     │1   │
        │                  ││───────────────────────────────────────────────────────┴────│
        │                  ││────────────────────────────────────────────────────────────│
        │ram               ││ 0                                                          │
        │                  ││────────────────────────────────────────────────────────────│
        │read_enable       ││                                       ┌─────┐              │
        │                  ││───────────────────────────────────────┘     └──────────────│
        │reset_read        ││                                                            │
        │                  ││────────────────────────────────────────────────────────────│
        │reset_write       ││                                                            │
        │                  ││────────────────────────────────────────────────────────────│
        │valid             ││                                 ┌───────────┐           ┌──│
        │                  ││─────────────────────────────────┘           └───────────┘  │
        │                  ││─────────────────────────────────┬───────────────────────┬──│
        │waddr_rd          ││ 0                               │1                      │3 │
        │                  ││─────────────────────────────────┴───────────────────────┴──│
        │                  ││───────────────────────────┬───────────────────────┬────────│
        │waddr_rd_ff_0     ││ 0                         │1                      │3       │
        │                  ││───────────────────────────┴───────────────────────┴────────│
        │                  ││─────────────────────────┬───────────────────┬──────────────│
        │waddr_wd          ││ 0                       │1                  │3             │
        │                  ││─────────────────────────┴───────────────────┴──────────────│
        │write_enable      ││               ┌─────────┐         ┌─────────┐              │
        │                  ││───────────────┘         └─────────┘         └──────────────│
        └──────────────────┘└────────────────────────────────────────────────────────────┘
        9d45c59d907202ea2c276ef7fb9afc29
        |}])
  ;;
end

(* Cyclesim tests *)
module _ = struct
  open Hardcaml_waveterm_cyclesim
  module Fifo_sim = Cyclesim.With_interface (Fifo.I) (Fifo.O)

  let on_cycle ~times f i = if i % times = 0 then f ~cycle:(i / times)

  let%expect_test "" =
    let config =
      { Cyclesim.Config.trace_all with
        clock_mode =
          By_input_clocks
            (Cyclesim_clock_domain.create_list [ "clock_read", 3; "clock_write", 5 ])
      }
    in
    let sim = Fifo_sim.create ~config (fifo ()) in
    let waves, sim = Waveform.create sim in
    let inputs = Cyclesim.inputs sim in
    let open Bits in
    let update_write ~cycle =
      let write_data_if ~cycle_is ~data =
        if cycle = cycle_is
        then (
          inputs.data_in <--. data;
          inputs.write_enable := Bits.vdd)
      in
      inputs.write_enable := Bits.gnd;
      write_data_if ~cycle_is:2 ~data:10;
      write_data_if ~cycle_is:4 ~data:11
    in
    let update_read ~cycle =
      if cycle = 7 then inputs.read_enable := Bits.vdd else inputs.read_enable := Bits.gnd
    in
    Cyclesim.cycle sim;
    for i = 0 to 35 do
      on_cycle ~times:5 update_write i;
      on_cycle ~times:3 update_read i;
      Cyclesim.cycle sim
    done;
    Waveform.print waves ~wave_width:0 ~display_width:90;
    [%expect
      {|
      ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
      │clock_read        ││  ┌─┐   ┌─┐   ┌─┐   ┌─┐   ┌─┐   ┌─┐   ┌─┐   ┌─┐   ┌─┐   ┌─┐   ┌─┐   │
      │                  ││──┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───┘ └───│
      │clock_write       ││  ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐       ┌─┐   │
      │                  ││──┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───────┘ └───│
      │                  ││──────────────────────┬───────────────────┬─────────────────────────│
      │data_in           ││ 0                    │A                  │B                        │
      │                  ││──────────────────────┴───────────────────┴─────────────────────────│
      │read_enable       ││                                            ┌─────┐                 │
      │                  ││────────────────────────────────────────────┘     └─────────────────│
      │reset_read        ││                                                                    │
      │                  ││────────────────────────────────────────────────────────────────────│
      │reset_write       ││                                                                    │
      │                  ││────────────────────────────────────────────────────────────────────│
      │write_enable      ││                      ┌─────────┐         ┌─────────┐               │
      │                  ││──────────────────────┘         └─────────┘         └───────────────│
      │almost_empty      ││────────────────────────────────────────────────────────────────────│
      │                  ││                                                                    │
      │                  ││──────────────────────────────────────┬───────────┬─────┬───────────│
      │data_out          ││ 0                                    │A          │0    │B          │
      │                  ││──────────────────────────────────────┴───────────┴─────┴───────────│
      │full              ││                                                                    │
      │                  ││────────────────────────────────────────────────────────────────────│
      │valid             ││                                            ┌─────┐           ┌─────│
      │                  ││────────────────────────────────────────────┘     └───────────┘     │
      │                  ││──────────────────────────────────────┬───────────┬─────┬───────────│
      │data_out_0        ││ 0                                    │A          │0    │B          │
      │                  ││──────────────────────────────────────┴───────────┴─────┴───────────│
      │gnd               ││                                                                    │
      │                  ││────────────────────────────────────────────────────────────────────│
      │                  ││──────────────────────────────────────────────────┬─────────────────│
      │raddr_rd          ││ 0                                                │1                │
      │                  ││──────────────────────────────────────────────────┴─────────────────│
      │                  ││──────────────────────────────────────────────────────────────┬─────│
      │raddr_wd          ││ 0                                                            │1    │
      │                  ││──────────────────────────────────────────────────────────────┴─────│
      │                  ││────────────────────────────────────────────────────┬───────────────│
      │raddr_wd_ff_0     ││ 0                                                  │1              │
      │                  ││────────────────────────────────────────────────────┴───────────────│
      │                  ││────────────────────────────────────────────┬─────────────────┬─────│
      │waddr_rd          ││ 0                                          │1                │3    │
      │                  ││────────────────────────────────────────────┴─────────────────┴─────│
      │                  ││──────────────────────────────────────┬─────────────────┬───────────│
      │waddr_rd_ff_0     ││ 0                                    │1                │3          │
      │                  ││──────────────────────────────────────┴─────────────────┴───────────│
      │                  ││────────────────────────────────┬───────────────────┬───────────────│
      │waddr_wd          ││ 0                              │1                  │3              │
      │                  ││────────────────────────────────┴───────────────────┴───────────────│
      └──────────────────┘└────────────────────────────────────────────────────────────────────┘
      |}]
  ;;
end
