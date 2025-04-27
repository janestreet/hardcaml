open Base
open Hardcaml

let tests =
  [ Bits.vdd
  ; Bits.gnd
  ; Bits.of_int_trunc ~width:8 0x24
  ; Bits.of_int32_trunc ~width:32 0xdeadbeefl
  ; Bits.of_int64_trunc ~width:64 Int64.min_value
  ; Bits.of_int64_trunc ~width:64 Int64.max_value
  ; Bits.of_string "65'h18000000000000000"
  ]
;;

let%expect_test "Bits.Binary.sexp_of_t examples" =
  tests
  |> List.map ~f:(fun b ->
    [%sexp { width = (Bits.width b : int); hex = (b : Bits.Binary.t) }])
  |> Expectable.print ~align:`right;
  [%expect
    {|
    ┌───────┬───────────────────────────────────────────────────────────────────────┐
    │ width │                                                                   hex │
    ├───────┼───────────────────────────────────────────────────────────────────────┤
    │     1 │                                                                  1'b1 │
    │     1 │                                                                  1'b0 │
    │     8 │                                                           8'b00100100 │
    │    32 │                                  32'b11011110101011011011111011101111 │
    │    64 │  64'b1000000000000000000000000000000000000000000000000000000000000000 │
    │    64 │  64'b0111111111111111111111111111111111111111111111111111111111111111 │
    │    65 │ 65'b11000000000000000000000000000000000000000000000000000000000000000 │
    └───────┴───────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "Bits.Hex.sexp_of_t examples" =
  tests
  |> List.map ~f:(fun b ->
    [%sexp { width = (Bits.width b : int); hex = (b : Bits.Hex.t) }])
  |> Expectable.print ~align:`right;
  [%expect
    {|
    ┌───────┬───────────────────────┐
    │ width │                   hex │
    ├───────┼───────────────────────┤
    │     1 │                  1'h1 │
    │     1 │                  1'h0 │
    │     8 │                 8'h24 │
    │    32 │          32'hdeadbeef │
    │    64 │  64'h8000000000000000 │
    │    64 │  64'h7fffffffffffffff │
    │    65 │ 65'h18000000000000000 │
    └───────┴───────────────────────┘
    |}]
;;

let%expect_test "Bits.Unsigned_int.sexp_of_t examples" =
  tests
  |> List.map ~f:(fun b ->
    [%sexp { width = (Bits.width b : int); hex = (b : Bits.Unsigned_int.t) }])
  |> Expectable.print ~align:`right;
  [%expect
    {|
    ┌───────┬──────────────────────────┐
    │ width │                      hex │
    ├───────┼──────────────────────────┤
    │     1 │                     1'd1 │
    │     1 │                     1'd0 │
    │     8 │                    8'd36 │
    │    32 │           32'd3735928559 │
    │    64 │  64'd9223372036854775808 │
    │    64 │  64'd9223372036854775807 │
    │    65 │ 65'd27670116110564327424 │
    └───────┴──────────────────────────┘
    |}]
;;

let%expect_test "Bits.Signed_int.sexp_of_t examples" =
  tests
  |> List.map ~f:(fun b ->
    [%sexp { width = (Bits.width b : int); hex = (b : Bits.Signed_int.t) }])
  |> Expectable.print ~align:`right;
  [%expect
    {|
    ┌───────┬──────────────────────────┐
    │ width │                      hex │
    ├───────┼──────────────────────────┤
    │     1 │                    1'd-1 │
    │     1 │                     1'd0 │
    │     8 │                    8'd36 │
    │    32 │           32'd-559038737 │
    │    64 │ 64'd-9223372036854775808 │
    │    64 │  64'd9223372036854775807 │
    │    65 │ 65'd-9223372036854775808 │
    └───────┴──────────────────────────┘
    |}]
;;

let%expect_test "to_string" =
  let bits = Bits.of_string "11" in
  Stdio.print_endline
    [%string
      "%{bits#Bits.Binary} %{bits#Bits.Hex} %{bits#Bits.Unsigned_int} \
       %{bits#Bits.Signed_int}"];
  [%expect {| 2'b11 2'h3 2'd3 2'd-1 |}]
;;
