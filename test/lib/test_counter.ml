open Core
open Hardcaml
open Hardcaml_waveterm_cyclesim

let%expect_test "incr/decr" =
  let test f x = Bits.of_unsigned_int ~width:3 x |> f |> Bits.to_unsigned_int in
  let incr = List.init 8 ~f:(test Bits.incr) in
  print_s [%message (incr : int list)];
  let decr = List.init 8 ~f:(test Bits.decr) in
  print_s [%message (decr : int list)];
  [%expect
    {|
    (incr (1 2 3 4 5 6 7 0))
    (decr (7 0 1 2 3 4 5 6))
    |}]
;;

let%expect_test "counter" =
  let open Signal in
  let test f =
    let clock = input "clock" 1 in
    let clear = input "clear" 1 in
    let enable = input "enable" 1 in
    let spec = Reg_spec.create ~clock ~clear () in
    let sim =
      Cyclesim.create (Circuit.create_exn ~name:"counter" [ output "q" (f spec enable) ])
    in
    let waves, sim = Waveform.create sim in
    let enable =
      try Cyclesim.in_port sim "enable" with
      | _ -> ref Bits.gnd
    in
    let clear = Cyclesim.in_port sim "clear" in
    enable := Bits.vdd;
    Cyclesim.cycle ~n:3 sim;
    enable := Bits.gnd;
    Cyclesim.cycle ~n:2 sim;
    enable := Bits.vdd;
    Cyclesim.cycle ~n:3 sim;
    clear := Bits.vdd;
    Cyclesim.cycle sim;
    clear := Bits.gnd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Waveform.print ~wave_width:1 waves
  in
  test (fun spec _enable -> counter spec ~width:4 ~by:1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││                                ┌───┐              │
    │               ││────────────────────────────────┘   └───────       │
    │               ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───       │
    │q              ││ 0  │1  │2  │3  │4  │5  │6  │7  │8  │0  │1         │
    │               ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───       │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  test (fun spec _enable -> counter ~initialize_to:(Bits.ones 4) spec ~width:4 ~by:1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││                                ┌───┐              │
    │               ││────────────────────────────────┘   └───────       │
    │               ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───       │
    │q              ││ F  │0  │1  │2  │3  │4  │5  │6  │7  │0  │1         │
    │               ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───       │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  test (fun spec _enable -> counter ~clear_to:(ones 4) spec ~width:4 ~by:1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││                                ┌───┐              │
    │               ││────────────────────────────────┘   └───────       │
    │               ││────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───       │
    │q              ││ 0  │1  │2  │3  │4  │5  │6  │7  │8  │F  │0         │
    │               ││────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───       │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  test (fun spec enable -> counter spec ~enable ~width:4 ~by:1);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││                                ┌───┐              │
    │               ││────────────────────────────────┘   └───────       │
    │enable         ││────────────┐       ┌───────────────────────       │
    │               ││            └───────┘                              │
    │               ││────┬───┬───┬───────────┬───┬───┬───┬───┬───       │
    │q              ││ 0  │1  │2  │3          │4  │5  │6  │0  │1         │
    │               ││────┴───┴───┴───────────┴───┴───┴───┴───┴───       │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  test (fun spec enable -> counter spec ~enable ~width:4 ~by:(-1));
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐│
    │               ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └│
    │clear          ││                                ┌───┐              │
    │               ││────────────────────────────────┘   └───────       │
    │enable         ││────────────┐       ┌───────────────────────       │
    │               ││            └───────┘                              │
    │               ││────┬───┬───┬───────────┬───┬───┬───┬───┬───       │
    │q              ││ 0  │F  │E  │D          │C  │B  │A  │0  │F         │
    │               ││────┴───┴───┴───────────┴───┴───┴───┴───┴───       │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
