open! Import

let%bench_fun "rac 4 coefs" =
  let module Config = struct
    let mode = Rac.Mode.Fixed
    let accumulator_bits = 20
    let data_bits = 8
    let num_coefs = 4
    let rom_shift = 6
  end in
  let module Rac = Hardcaml_examples_tests.Test_integer_rac.Make(Config) in
  let open Rac in

  let simulator = create_sim ~coefs:[|1;2;3;4|] in
  let data_in = [|1;2;2;4|] in
  fun () -> run ~simulator ~testbench ~data_in

let%bench_fun "rac 8 coefs" =
  let module Config = struct
    let mode = Rac.Mode.Fixed
    let accumulator_bits = 20
    let data_bits = 8
    let num_coefs = 8
    let rom_shift = 6
  end in
  let module Rac = Hardcaml_examples_tests.Test_integer_rac.Make(Config) in
  let open Rac in
  let simulator = create_sim ~coefs:[|1;2;3;4;5;6;7;8|] in
  let data_in = [|1;2;2;4;5;6;7;8|] in
  fun () -> run ~simulator ~testbench ~data_in
