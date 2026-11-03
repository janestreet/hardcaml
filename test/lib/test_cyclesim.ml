open! Core
open! Hardcaml
open Signal
open Expect_test_helpers_base

let%expect_test "Initial values, resets and clears of registers" =
  let clock = input "clock" 1 in
  let reset = input "reset" 1 in
  let clear = input "clear" 1 in
  let q =
    reg_fb
      (Reg_spec.create ~clock ~reset ~clear ())
      ~initialize_to:(Bits.of_int_trunc ~width:8 16)
      ~reset_to:(Bits.of_int_trunc ~width:8 32)
      ~clear_to:(Signal.of_int_trunc ~width:8 48)
      ~width:8
      ~f:(fun d -> d +:. 1)
  in
  let circ = Circuit.create_exn ~name:"initial" [ output "q" q ] in
  let sim = Cyclesim.create circ in
  let waves, sim = Cyclesim.Waveform.create sim in
  (* This is an explicit for-loop here to test that the default value of [n] is 1 *)
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Cyclesim.reset sim;
  Cyclesim.cycle ~n:2 sim;
  Cyclesim.in_port sim "clear" := Bits.vdd;
  Cyclesim.cycle sim;
  Cyclesim.in_port sim "clear" := Bits.gnd;
  Cyclesim.cycle ~n:2 sim;
  Hardcaml_waveterm_kernel.Waveform.print waves ~wave_scale:2;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐     │
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──   │
    │reset          ││            ┌─────┐                                │
    │               ││────────────┘     └─────────────────────────────   │
    │clear          ││                              ┌─────┐              │
    │               ││──────────────────────────────┘     └───────────   │
    │               ││──────┬───────────┬─────┬─────┬─────┬─────┬─────   │
    │q              ││ 10   │11         │20   │21   │22   │30   │31      │
    │               ││──────┴───────────┴─────┴─────┴─────┴─────┴─────   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "reg_fb_and_next returns registered value and next cycle value" =
  let clock = input "clock" 1 in
  let reset = input "reset" 1 in
  let clear = input "clear" 1 in
  let q, next =
    reg_fb_and_next
      (Reg_spec.create ~clock ~reset ~clear ())
      ~initialize_to:(Bits.of_int_trunc ~width:8 16)
      ~reset_to:(Bits.of_int_trunc ~width:8 32)
      ~clear_to:(Signal.of_int_trunc ~width:8 48)
      ~width:8
      ~f:(fun d -> d +:. 1)
  in
  let circ =
    Circuit.create_exn ~name:"reg_fb_and_next" [ output "q" q; output "next" next ]
  in
  let sim = Cyclesim.create circ in
  let waves, sim = Cyclesim.Waveform.create sim in
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Cyclesim.reset sim;
  Cyclesim.cycle ~n:2 sim;
  Cyclesim.in_port sim "clear" := Bits.vdd;
  Cyclesim.cycle sim;
  Cyclesim.in_port sim "clear" := Bits.gnd;
  Cyclesim.cycle ~n:2 sim;
  Hardcaml_waveterm_kernel.Waveform.print waves ~wave_scale:2;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││───┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐     │
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──   │
    │reset          ││            ┌─────┐                                │
    │               ││────────────┘     └─────────────────────────────   │
    │clear          ││                              ┌─────┐              │
    │               ││──────────────────────────────┘     └───────────   │
    │               ││──────┬───────────┬─────┬─────┬─────┬─────┬─────   │
    │q              ││ 10   │11         │20   │21   │22   │30   │31      │
    │               ││──────┴───────────┴─────┴─────┴─────┴─────┴─────   │
    │               ││──────┬───────────┬─────┬─────┬─────┬─────┬─────   │
    │next           ││ 11   │12         │21   │22   │23   │31   │32      │
    │               ││──────┴───────────┴─────┴─────┴─────┴─────┴─────   │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "timeout" =
  let circ = Circuit.create_exn ~name:"timeout" [ output "q" vdd ] in
  let sim =
    Cyclesim.create circ
    |> Cyclesim.raise_after_timeout ~message:"custom timeout message" ~timeout:2
  in
  Cyclesim.cycle sim;
  [%expect {| |}];
  Cyclesim.cycle sim;
  [%expect {| |}];
  require_does_raise ~hide_positions:true (fun () -> Cyclesim.cycle sim);
  [%expect
    {|
    ("Cyclesim timed out" "custom timeout message"
      (timeout 2)
      (timeout_set_at lib/hardcaml/hardcaml/test/lib/test_cyclesim.ml:LINE:COL))
    |}]
;;

let%expect_test "nested timeouts" =
  (* You can nest timeouts - the inner timeout sim will still tick the outer timeout sim -
     except on the cycle it raises.

     I wouldn't rely too heavily on this as the exact behaviour is quite complicated. *)
  let circ = Circuit.create_exn ~name:"timeout" [ output "q" vdd ] in
  let sim =
    let here = Source_code_position.of_pos ("outer", 0, 0, 0) in
    Cyclesim.create circ |> Cyclesim.raise_after_timeout ~here ~timeout:10
  in
  Cyclesim.cycle ~n:5 sim;
  let sim' =
    let here = Source_code_position.of_pos ("inner", 0, 0, 0) in
    Cyclesim.raise_after_timeout ~here sim ~timeout:3
  in
  Cyclesim.cycle ~n:3 sim';
  [%expect {| |}];
  require_does_raise ~hide_positions:true (fun () -> Cyclesim.cycle sim');
  [%expect
    {|
    ("Cyclesim timed out"
      (timeout        3)
      (timeout_set_at inner:LINE:COL))
    |}];
  Cyclesim.cycle ~n:2 sim;
  require_does_raise ~hide_positions:true (fun () -> Cyclesim.cycle sim);
  [%expect
    {|
    ("Cyclesim timed out"
      (timeout        10)
      (timeout_set_at outer:LINE:COL))
    |}];
  (* Reset the timeouts *)
  Cyclesim.reset sim;
  (* inner timeout does not raise if not tickled. *)
  Cyclesim.cycle ~n:3 sim';
  Cyclesim.cycle ~n:7 sim;
  require_does_raise ~hide_positions:true (fun () -> Cyclesim.cycle sim);
  [%expect
    {|
    ("Cyclesim timed out"
      (timeout        10)
      (timeout_set_at outer:LINE:COL))
    |}]
;;

let%expect_test "synthesized clocks share a single wave, however they are named" =
  (* In (non-multi-clock) cyclesim there is really only one clock, so every clock signal
     - input ports and internal aliases alike - shares one synthesized wave. *)
  let clock = input "clock" 1 in
  let clock2 = input "clock2" 1 in
  let clock_inner = wireof clock -- "clock_inner" in
  let q = reg (Reg_spec.create ~clock:clock_inner ()) (input "d" 8) in
  let q2 = reg (Reg_spec.create ~clock:clock2 ()) (input "d2" 8) in
  let circ = Circuit.create_exn ~name:"clocks" [ output "q" q; output "q2" q2 ] in
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circ in
  let waves, sim = Cyclesim.Waveform.create sim in
  Cyclesim.cycle ~n:3 sim;
  (match waves with
   | By_mutable_event waves ->
     let find name = Array.find_exn waves ~f:(fun wave -> String.equal wave.name name) in
     let clock_wave = find "clock" in
     List.iter [ "clock2"; "clock_inner" ] ~f:(fun name ->
       require
         (phys_equal clock_wave.wave_data (find name).wave_data)
         ~if_false_then_print_s:
           (lazy [%message "clock wave data is not shared" (name : string)]))
   | By_mutable_event_four_state _ -> raise_s [%message "expected mutable event waves"]);
  Hardcaml_waveterm_kernel.Waveform.print waves ~wave_scale:2;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││───┐  ┌──┐  ┌──┐                                   │
    │               ││   └──┘  └──┘  └──                                 │
    │clock2         ││───┐  ┌──┐  ┌──┐                                   │
    │               ││   └──┘  └──┘  └──                                 │
    │               ││──────────────────                                 │
    │d2             ││ 00                                                │
    │               ││──────────────────                                 │
    │               ││──────────────────                                 │
    │d              ││ 00                                                │
    │               ││──────────────────                                 │
    │               ││──────────────────                                 │
    │q              ││ 00                                                │
    │               ││──────────────────                                 │
    │               ││──────────────────                                 │
    │q2             ││ 00                                                │
    │               ││──────────────────                                 │
    │clock_inner    ││───┐  ┌──┐  ┌──┐                                   │
    │               ││   └──┘  └──┘  └──                                 │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
