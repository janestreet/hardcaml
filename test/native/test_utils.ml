open Hardcaml
open Hardcaml_test.Test_utils

let%expect_test "roundtrips" =
  let int = 63, fun width x -> x |> Constant.to_int |> Constant.of_int ~width in
  test_round_trip int
;;
