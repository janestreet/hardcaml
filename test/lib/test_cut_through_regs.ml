open Core
open Hardcaml
open Signal
open Hardcaml_waveterm_cyclesim
module R = (val Types.value 8)

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; enable : 'a
    ; d : 'a R.t
    }
  [@@deriving hardcaml ~rtlmangle:true]
end

module O = struct
  type 'a t =
    { q : 'a [@bits 8]
    ; q_clocking : 'a [@bits 8]
    ; q_intf : 'a R.t
    ; q_always : 'a [@bits 8]
    ; q_clocking_always : 'a [@bits 8]
    ; q_always_intf : 'a R.t
    }
  [@@deriving hardcaml ~rtlmangle:true]
end

let%expect_test "signal" =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim =
    Sim.create (fun { clock; clear; enable; d } ->
      let spec = Reg_spec.create ~clock ~clear () in
      let clocking = { Clocking.clock; clear } in
      let clear_to = Signal.of_unsigned_int ~width:8 0xb in
      let q = cut_through_reg spec ~enable ~clear_to d in
      let q_clocking = Clocking.cut_through_reg clocking ~enable ~clear_to d in
      let q_intf = R.Of_signal.cut_through_reg spec ~enable ~clear_to d in
      let counter = Always.Variable.reg spec ~width:8 in
      let q_always = Always.Variable.cut_through_reg spec ~clear_to ~width:8 in
      let q_clocking_always = Clocking.Var.cut_through_reg clocking ~clear_to ~width:8 in
      let q_always_intf = R.Of_always.cut_through_reg spec ~clear_to in
      Always.(
        compile
          [ counter <-- counter.value +:. 1
          ; when_ enable [ q_always <-- d ]
          ; when_ enable [ q_clocking_always <-- d ]
          ; when_ enable [ R.Of_always.assign q_always_intf d ]
          ]);
      { O.q
      ; q_clocking
      ; q_intf
      ; q_always = q_always.value
      ; q_clocking_always = q_clocking_always.value
      ; q_always_intf = R.Of_always.value q_always_intf
      })
  in
  let waves, sim = Waveform.create sim in
  let inputs = Cyclesim.inputs sim in
  let open Bits in
  (* Show behaviour wrt to clear and enable *)
  inputs.clear <--. 1;
  inputs.d <--. 10;
  Cyclesim.cycle sim;
  inputs.enable <--. 1;
  Cyclesim.cycle sim;
  inputs.clear <--. 0;
  (* runtime *)
  for i = 0 to 4 do
    inputs.d <--. i;
    inputs.enable := Bits.of_bool (i % 2 = 1);
    Cyclesim.cycle sim
  done;
  Waveform.print waves ~wave_width:2;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │clear          ││────────────┐                                      │
    │               ││            └─────────────────────────────         │
    │enable         ││      ┌─────┐     ┌─────┐     ┌─────┐              │
    │               ││──────┘     └─────┘     └─────┘     └─────         │
    │               ││────────────┬─────┬─────┬─────┬─────┬─────         │
    │d$value        ││ 0A         │00   │01   │02   │03   │04            │
    │               ││────────────┴─────┴─────┴─────┴─────┴─────         │
    │               ││──────┬─────┬─────┬───────────┬───────────         │
    │q              ││ 00   │0A   │0B   │01         │03                  │
    │               ││──────┴─────┴─────┴───────────┴───────────         │
    │               ││──────┬─────┬─────┬───────────┬───────────         │
    │q_always       ││ 00   │0A   │0B   │01         │03                  │
    │               ││──────┴─────┴─────┴───────────┴───────────         │
    │               ││──────┬─────┬─────┬───────────┬───────────         │
    │q_always_intf$v││ 00   │0A   │0B   │01         │03                  │
    │               ││──────┴─────┴─────┴───────────┴───────────         │
    │               ││──────┬─────┬─────┬───────────┬───────────         │
    │q_clocking     ││ 00   │0A   │0B   │01         │03                  │
    │               ││──────┴─────┴─────┴───────────┴───────────         │
    │               ││──────┬─────┬─────┬───────────┬───────────         │
    │q_clocking_alwa││ 00   │0A   │0B   │01         │03                  │
    │               ││──────┴─────┴─────┴───────────┴───────────         │
    │               ││──────┬─────┬─────┬───────────┬───────────         │
    │q_intf$value   ││ 00   │0A   │0B   │01         │03                  │
    │               ││──────┴─────┴─────┴───────────┴───────────         │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
