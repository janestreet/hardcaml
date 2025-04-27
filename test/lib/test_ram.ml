open Core
open Hardcaml
open Hardcaml_waveterm_cyclesim

let%expect_test "simple dual port" =
  let module Sdp =
    Ram.Dual_port.Make (struct
      let address_bits = 2
      let data_bits = 8
    end)
  in
  let module Sim = Cyclesim.With_interface (Sdp.I) (Sdp.O) in
  let scope = Scope.create ~flatten_design:true () in
  let sim = Sim.create (Sdp.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let open Bits in
  let cycle () =
    Cyclesim.cycle sim;
    inputs.port_a.write := gnd;
    inputs.port_b.write := gnd;
    inputs.port_a.enable := gnd;
    inputs.port_b.enable := gnd
  in
  let write (port : _ Sdp.Port.t) address data =
    port.write := vdd;
    port.enable := vdd;
    port.address <--. address;
    port.data <--. data
  in
  let read (port : _ Sdp.Port.t) address =
    port.enable := vdd;
    port.address <--. address
  in
  (* write on port a *)
  write inputs.port_a 3 10;
  cycle ();
  (* read on port a *)
  read inputs.port_a 3;
  cycle ();
  (* write on port b *)
  write inputs.port_b 2 20;
  cycle ();
  (* read on port b *)
  read inputs.port_b 2;
  cycle ();
  cycle ();
  (* read on both ports; swap addresses *)
  read inputs.port_a 2;
  read inputs.port_b 3;
  cycle ();
  cycle ();
  Waveform.print waves ~wave_width:1;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │               ││────────────────────┬───────                       │
    │port_a_address ││ 3                  │2                             │
    │               ││────────────────────┴───────                       │
    │               ││────────────────────────────                       │
    │port_a_data    ││ 0A                                                │
    │               ││────────────────────────────                       │
    │port_a_enable  ││────────┐           ┌───┐                          │
    │               ││        └───────────┘   └───                       │
    │port_a_write   ││────┐                                              │
    │               ││    └───────────────────────                       │
    │               ││────────┬───────────┬───────                       │
    │port_b_address ││ 0      │2          │3                             │
    │               ││────────┴───────────┴───────                       │
    │               ││────────┬───────────────────                       │
    │port_b_data    ││ 00     │14                                        │
    │               ││────────┴───────────────────                       │
    │port_b_enable  ││        ┌───────┐   ┌───┐                          │
    │               ││────────┘       └───┘   └───                       │
    │port_b_write   ││        ┌───┐                                      │
    │               ││────────┘   └───────────────                       │
    │               ││────────┬───────────────┬───                       │
    │qa             ││ 00     │0A             │14                        │
    │               ││────────┴───────────────┴───                       │
    │               ││────────────────┬───────┬───                       │
    │qb             ││ 00             │14     │0A                        │
    │               ││────────────────┴───────┴───                       │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
