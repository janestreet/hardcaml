open! Import

module Make (Config : Rac.Config) = struct
  module Rac  = Rac.Make(Config)
  module Step = Hardcaml_step_testbench.Make(Rac.I)(Rac.O)
  module Sim  = Cyclesim.With_interface(Rac.I)(Rac.O)

  let testbench ~data_in _ =
    let open Step.Let_syntax in
    let is_integer_mode = match Config.mode with Integer -> Bits.vdd | Fixed -> Bits.gnd in
    let i : Bits.t Rac.I.t = Rac.I.map Rac.I.t ~f:(fun _ -> Bits.empty) in
    let%bind _ = Step.cycle { i with clr = Bits.vdd } in
    let%bind _ = Step.cycle { i with en = Bits.vdd
                                   ; ld = Bits.vdd
                                   ; x = Array.map ~f:(Bits.consti 8) data_in } in
    (* This cycle's [addsub] setting is because in [Integer] mode, the first bit that we
       process is the (negative) high bit in the twos-complement integer, so we want the
       accumulator to subtract rather than add. *)
    let%bind _ = Step.cycle { i with addsub = is_integer_mode } in
    let%bind _ = Step.cycle ~num_cycles:(Config.data_bits - 2) i in
    (* This cycle's [addsub] setting is because in [Fixed] mode, the last bit that we
       process is the (negative) high bit in the twos-complement fixnum, so we
       accumulator to subtract rather than add. *)
    let%bind o = Step.cycle { i with addsub = Bits.(~:) is_integer_mode } in
    (* The result is available above if the testbench outputs are calculated [After] the
       clock edge, otherwise below.  Either way we need to final step to display the data
       in the waveform. *)
    let%bind _ = Step.cycle i in
    return (Bits.to_sint o.q)

  let run ~simulator ~testbench ~data_in =
    Step.run_until_finished
      ~simulator
      ~testbench:(testbench ~data_in)
      ~input_default:{ Step.input_zero with en = Bits.empty }
      ()

  let create_sim ~coefs =
    let coefs = Array.map coefs ~f:(Bits.consti 8) in
    Sim.create ~kind:Mutable (Rac.create ~coefs)

  let run_and_print_waves ~simulator ~testbench ~data_in =
    let waves, simulator = Hardcaml_waveterm_jane.Waveform.create simulator in
    let result = run ~simulator ~testbench:testbench ~data_in in
    Hardcaml_waveterm_jane.Waveform.print
      ~display_height:28
      ~display_width:120
      ~wave_width:2
      waves;
    result

  let test ?(print=false) () ~coefs ~data_in =
    let simulator = create_sim ~coefs in
    (if print then run_and_print_waves else run)
      ~simulator ~testbench ~data_in

end

module Config = struct
  let mode = Rac.Mode.Integer
  let accumulator_bits = 20
  let data_bits = 8
  let num_coefs = 4
  let rom_shift = 0
end

include Make(Config)

let test ?print () ~coefs ~data_in =
  let eval ~coefs ~data_in =
    Array.map2_exn coefs data_in ~f:( * ) |> Array.reduce_exn ~f:(+) in
  print_s [%message "" ~expected:(eval ~coefs ~data_in : int)];
  let result = test ?print () ~coefs ~data_in in
  print_s [%message "" (result : int)]

let%expect_test "simulation example" =
  test ~print:true () ~coefs:[|1;2;3;4|] ~data_in:[|1;2;3;4|];
  [%expect {|
    (expected 30)
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────────────────────────────────────────────┐
    │clk               ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌─│
    │                  ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘ │
    │clr               ││──────┐                                                                                           │
    │                  ││      └─────────────────────────────────────────────────────────────────                          │
    │addsub            ││            ┌─────┐                                                                               │
    │                  ││────────────┘     └─────────────────────────────────────────────────────                          │
    │en                ││      ┌─────────────────────────────────────────────────────────────────                          │
    │                  ││──────┘                                                                                           │
    │ld                ││      ┌─────┐                                                                                     │
    │                  ││──────┘     └───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x0                ││ 00   │01   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x1                ││ 00   │02   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x2                ││ 00   │03   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││──────┬─────┬───────────────────────────────────────────────────────────                          │
    │x3                ││ 00   │04   │00                                                                                   │
    │                  ││──────┴─────┴───────────────────────────────────────────────────────────                          │
    │                  ││────────────────────────────────────────────────┬─────┬─────┬─────┬─────                          │
    │q                 ││ 00000                                          │00004│0000D│0001E│0003C                          │
    │                  ││────────────────────────────────────────────────┴─────┴─────┴─────┴─────                          │
    │                  ││                                                                                                  │
    └──────────────────┘└──────────────────────────────────────────────────────────────────────────────────────────────────┘
    (result 30) |}];
;;

let%expect_test "tests" =
  test () ~coefs:[|3; 6; 1; 2|] ~data_in:[|7; 9; 3; 5|];
  [%expect {|
    (expected 88)
    (result 88) |}];
  test () ~coefs:[|33; 26; 61; 12|] ~data_in:[|17; 39; 43; 15|];
  [%expect {|
    (expected 4378)
    (result 4378) |}];
  (* signed data *)
  test () ~coefs:[|33; 26; 61; 12|] ~data_in:[|17; -39; 43; 15|];
  [%expect {|
    (expected 2350)
    (result 2350) |}];
  (* signed coefficient *)
  test () ~coefs:[|33; 26; -61; 12|] ~data_in:[|17; 39; 43; 15|];
  [%expect {|
    (expected -868)
    (result -868) |}];
  (* signed data and coefficients *)
  test () ~coefs:[|-33; 26; -61; 12|] ~data_in:[|17; 39; -43; -15|];
  [%expect {|
    (expected 2896)
    (result 2896) |}];
;;
