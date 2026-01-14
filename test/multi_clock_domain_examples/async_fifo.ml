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

module%test [@tags "runtime5-only"] Cyclesim_tests = struct
  open Hardcaml_waveterm.For_cyclesim
  module Fifo_sim = Cyclesim.With_interface (Fifo.I) (Fifo.O)

  let sim () =
    let config =
      { Cyclesim.Config.trace_all with
        clock_mode =
          By_input_clocks
            (Cyclesim_clock_domain.create_list [ "clock_read", 3; "clock_write", 5 ])
      }
    in
    Fifo_sim.create ~config (fifo ())
  ;;

  module%test Test_direct = struct
    let on_cycle ~times f i = if i % times = 0 then f ~cycle:(i / times)

    let%expect_test "" =
      let sim = sim () in
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
        if cycle = 7
        then inputs.read_enable := Bits.vdd
        else inputs.read_enable := Bits.gnd
      in
      for i = 0 to 40 do
        on_cycle ~times:5 update_write i;
        on_cycle ~times:3 update_read i;
        Cyclesim.cycle sim
      done;
      Waveform.print waves ~wave_width:(-1) ~display_width:90;
      [%expect
        {|
        ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────┐
        │clock_read        ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
        │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
        │clock_write       ││─────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐  │
        │                  ││     └────┘    └────┘    └────┘    └────┘    └────┘    └────┘    └──│
        │                  ││────────────────────┬───────────────────┬───────────────────────────│
        │data_in           ││ 0                  │A                  │B                          │
        │                  ││────────────────────┴───────────────────┴───────────────────────────│
        │read_enable       ││                                          ┌─────┐                   │
        │                  ││──────────────────────────────────────────┘     └───────────────────│
        │reset_read        ││                                                                    │
        │                  ││────────────────────────────────────────────────────────────────────│
        │reset_write       ││                                                                    │
        │                  ││────────────────────────────────────────────────────────────────────│
        │write_enable      ││                    ┌─────────┐         ┌─────────┐                 │
        │                  ││────────────────────┘         └─────────┘         └─────────────────│
        │almost_empty      ││────────────────────────────────────────────────────────────────────│
        │                  ││                                                                    │
        │                  ││────────────────────────────────────┬───────────┬─────┬─────────────│
        │data_out          ││ 0                                  │A          │0    │B            │
        │                  ││────────────────────────────────────┴───────────┴─────┴─────────────│
        │full              ││                                                                    │
        │                  ││────────────────────────────────────────────────────────────────────│
        │valid             ││                                          ┌─────┐           ┌───────│
        │                  ││──────────────────────────────────────────┘     └───────────┘       │
        │                  ││────────────────────────────────────┬───────────┬─────┬─────────────│
        │data_out_0        ││ 0                                  │A          │0    │B            │
        │                  ││────────────────────────────────────┴───────────┴─────┴─────────────│
        │gnd               ││                                                                    │
        │                  ││────────────────────────────────────────────────────────────────────│
        │                  ││────────────────────────────────────────────────┬───────────────────│
        │raddr_rd          ││ 0                                              │1                  │
        │                  ││────────────────────────────────────────────────┴───────────────────│
        │                  ││────────────────────────────────────────────────────────────┬───────│
        │raddr_wd          ││ 0                                                          │1      │
        │                  ││────────────────────────────────────────────────────────────┴───────│
        │                  ││──────────────────────────────────────────────────┬─────────────────│
        │raddr_wd_ff_0     ││ 0                                                │1                │
        │                  ││──────────────────────────────────────────────────┴─────────────────│
        │                  ││──────────────────────────────────────────┬─────────────────┬───────│
        │waddr_rd          ││ 0                                        │1                │3      │
        │                  ││──────────────────────────────────────────┴─────────────────┴───────│
        │                  ││────────────────────────────────────┬─────────────────┬─────────────│
        │waddr_rd_ff_0     ││ 0                                  │1                │3            │
        │                  ││────────────────────────────────────┴─────────────────┴─────────────│
        │                  ││──────────────────────────────┬───────────────────┬─────────────────│
        │waddr_wd          ││ 0                            │1                  │3                │
        │                  ││──────────────────────────────┴───────────────────┴─────────────────│
        └──────────────────┘└────────────────────────────────────────────────────────────────────┘
        |}]
    ;;
  end

  module%test Test_step_testbench = struct
    let run f ~count =
      for _ = 0 to count - 1 do
        let results = f () in
        print_endline [%string {| ---> %{results#Bits}|}];
        print_endline ""
      done
    ;;

    module Functional = struct
      module Step =
        Hardcaml_step_testbench_effectful.Functional.Cyclesim.Make (Fifo.I) (Fifo.O)

      type finished_event = (unit, Step.I_data.t) Step.finished_event

      let run_test testbench ~count ~print_waves =
        let simulator = sim () in
        let waves, simulator = Waveform.create simulator in
        let f () = Step.run_until_finished () ~show_steps:true ~simulator ~testbench in
        run f ~count;
        if print_waves then Waveform.print waves ~wave_width:(-1)
      ;;

      let read_write_test ~count ~print_waves =
        let write_one handler _ =
          print_s [%message "start write"];
          Step.delay
            ~num_cycles:1
            handler
            { Step.input_hold with
              data_in = Bits.of_int_trunc ~width:4 5
            ; write_enable = Bits.vdd
            };
          print_s [%message "end write"];
          Step.delay
            ~num_cycles:1
            handler
            { Step.input_hold with data_in = Bits.zero 4; write_enable = Bits.gnd }
        in
        let read_one (handler @ local) _ =
          let o =
            ref (Step.cycle handler { Step.input_hold with read_enable = Bits.vdd })
          in
          while
            print_s [%message "check read"];
            not (Bits.to_bool !o.before_edge.valid)
          do
            o := Step.cycle handler Step.input_hold
          done;
          Step.delay ~num_cycles:1 handler { Step.input_hold with read_enable = Bits.gnd };
          !o.before_edge.data_out
        in
        run_test ~count ~print_waves (fun handler _ ->
          let result_event = Step.spawn handler read_one ~period:3 in
          let _ : finished_event = Step.spawn handler write_one ~period:5 in
          Step.wait_for handler result_event)
      ;;
    end

    module Imperative = struct
      module Step = Hardcaml_step_testbench_effectful.Imperative.Cyclesim

      type finished_event = (unit, Step.I_data.t) Step.finished_event

      let run_test testbench ~count ~print_waves =
        let simulator = sim () in
        let waves, simulator = Waveform.create simulator in
        let inputs = Cyclesim.inputs simulator in
        let outputs = Cyclesim.outputs ~clock_edge:Before simulator in
        let f () =
          Step.run_until_finished
            ()
            ~show_steps:true
            ~simulator
            ~testbench:(fun handler -> testbench handler ~inputs ~outputs)
        in
        run f ~count;
        if print_waves then Waveform.print waves ~wave_width:(-1)
      ;;

      let read_write_test ~count ~print_waves =
        let write_one handler ~(inputs : _ Fifo.I.t) ~outputs:_ () =
          print_s [%message "start write"];
          inputs.data_in := Bits.of_int_trunc ~width:4 5;
          inputs.write_enable := Bits.vdd;
          Step.cycle handler ();
          print_s [%message "end write"];
          inputs.write_enable := Bits.gnd;
          inputs.data_in := Bits.zero 4;
          Step.cycle handler ()
        in
        let read_one (local_ handler) ~(inputs : _ Fifo.I.t) ~(outputs : _ Fifo.O.t) () =
          inputs.read_enable := Bits.vdd;
          Step.cycle handler ();
          let rec loop () =
            print_s [%message "check read"];
            if Bits.to_bool !(outputs.valid)
            then (
              let data_out = !(outputs.data_out) in
              inputs.read_enable := Bits.gnd;
              Step.cycle handler ();
              data_out)
            else (
              Step.cycle handler ();
              loop ())
          in
          loop () [@nontail]
        in
        run_test ~count ~print_waves (fun handler ~inputs ~outputs ->
          let result_event = Step.spawn handler (read_one ~inputs ~outputs) ~period:3 in
          let _ : finished_event =
            Step.spawn handler (write_one ~inputs ~outputs) ~period:5
          in
          Step.wait_for handler result_event)
      ;;
    end

    let%expect_test "test read and write" =
      List.iter [ Functional.read_write_test; Imperative.read_write_test ] ~f:(fun f ->
        f ~print_waves:true ~count:2;
        [%expect
          {|
          (step_number 0)
          "start write"
          (step_number 1)
          (step_number 2)
          (step_number 3)
          "check read"
          (step_number 4)
          (step_number 5)
          "end write"
          (step_number 6)
          "check read"
          (step_number 7)
          (step_number 8)
          (step_number 9)
          "check read"
          (step_number 10)
          (step_number 11)
          (step_number 12)
          "check read"
          (step_number 13)
          (step_number 14)
          (step_number 15)
          (step_number 16)
           ---> 0101

          (step_number 0)
          "start write"
          (step_number 1)
          (step_number 2)
          (step_number 3)
          "check read"
          (step_number 4)
          (step_number 5)
          "end write"
          (step_number 6)
          "check read"
          (step_number 7)
          (step_number 8)
          (step_number 9)
          "check read"
          (step_number 10)
          (step_number 11)
          (step_number 12)
          "check read"
          (step_number 13)
          (step_number 14)
          (step_number 15)
          (step_number 16)
           ---> 0101

          ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
          │clock_read     ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
          │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
          │clock_write    ││─────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌│
          │               ││     └────┘    └────┘    └────┘    └────┘    └────┘│
          │               ││──────────┬────────────────────────────────────────│
          │data_in        ││ 5        │0                                       │
          │               ││──────────┴────────────────────────────────────────│
          │read_enable    ││────────────────────────┐                          │
          │               ││                        └──────────────────────────│
          │reset_read     ││                                                   │
          │               ││───────────────────────────────────────────────────│
          │reset_write    ││                                                   │
          │               ││───────────────────────────────────────────────────│
          │write_enable   ││──────────┐                                        │
          │               ││          └────────────────────────────────────────│
          │almost_empty   ││───────────────────────────────────────────────────│
          │               ││                                                   │
          │               ││────────────┬───────────┬──────────────────────────│
          │data_out       ││ 0          │5          │0                         │
          │               ││────────────┴───────────┴──────────────────────────│
          │full           ││                                                   │
          │               ││───────────────────────────────────────────────────│
          │valid          ││                  ┌─────┐                          │
          │               ││──────────────────┘     └──────────────────────────│
          │               ││────────────┬───────────┬──────────────────────────│
          │data_out_0     ││ 0          │5          │0                         │
          │               ││────────────┴───────────┴──────────────────────────│
          │gnd            ││                                                   │
          │               ││───────────────────────────────────────────────────│
          │               ││────────────────────────┬──────────────────────────│
          │raddr_rd       ││ 0                      │1                         │
          │               ││────────────────────────┴──────────────────────────│
          │               ││────────────────────────────────────────┬──────────│
          │raddr_wd       ││ 0                                      │1         │
          │               ││────────────────────────────────────────┴──────────│
          │               ││──────────────────────────────┬────────────────────│
          │raddr_wd_ff_0  ││ 0                            │1                   │
          │               ││──────────────────────────────┴────────────────────│
          │               ││──────────────────┬────────────────────────────────│
          │waddr_rd       ││ 0                │1                               │
          │               ││──────────────────┴────────────────────────────────│
          │               ││────────────┬──────────────────────────────────────│
          │waddr_rd_ff_0  ││ 0          │1                                     │
          │               ││────────────┴──────────────────────────────────────│
          │               ││──────────┬────────────────────────────────────────│
          │waddr_wd       ││ 0        │1                                       │
          │               ││──────────┴────────────────────────────────────────│
          └───────────────┘└───────────────────────────────────────────────────┘
          |}])
    ;;
  end
end
