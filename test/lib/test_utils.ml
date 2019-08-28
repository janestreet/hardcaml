(* test various conversions in [Constant], and some other misc operations *)
open! Import

let%expect_test "list_of_string" =
  [%sexp (String.to_list "123abc" : char list)] |> print_s;
  [%expect {| (1 2 3 a b c) |}]
;;

let%expect_test "int_of_hchar" =
  [%sexp
    (List.map ~f:Constant.int_of_hex_char @@ String.to_list "0123456789abcdefABCDEF"
     : int list)]
  |> print_s;
  [%expect {| (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 10 11 12 13 14 15) |}]
;;

let%expect_test "int_of_hchar raises" =
  require_does_raise [%here] (fun () -> Constant.int_of_hex_char 'g');
  [%expect {| ("Invalid hex char" g) |}]
;;

let%expect_test "[of_binary_string] raises" =
  let to_bstr s = Constant.of_binary_string s in
  require_does_raise [%here] ~cr:CR_soon (fun () -> to_bstr "");
  [%expect {|
    "[Constant.of_binary_string] input string is empty" |}];
  require_does_raise [%here] ~cr:CR_soon (fun () -> to_bstr "z");
  [%expect
    {|
    ("[Constant.of_binary_string] input must only consist of '1' or '0'" (got z)) |}]
;;

let%expect_test "roundtrips" =
  let max_bit_width = 200 in
  (* Exhaustive test from 1 to 10 bits *)
  let test_round_trip_int (_, round_trip) =
    let round_trip width x =
      x |> Constant.of_int ~width |> round_trip width |> Constant.to_int
    in
    for width = 1 to 10 do
      for i = 0 to (1 lsl width) - 1 do
        require_equal [%here] (module Int) i (i |> round_trip width)
      done
    done
  in
  (* random tests up to [max_bit_width] *)
  let test_round_trip_string (max_bits, round_trip) =
    let round_trip width x =
      x |> Constant.of_binary_string |> round_trip width |> Constant.to_binary_string
    in
    for bit_width = 1 to max_bits do
      for _ = 0 to 99 do
        (* 100 random strings at this width *)
        let s =
          String.init bit_width ~f:(fun _ -> if Random.int 2 = 0 then '0' else '1')
        in
        require_equal [%here] (module String) s (round_trip bit_width s)
      done
    done
  in
  let test_round_trip t =
    test_round_trip_int t;
    test_round_trip_string t
  in
  let int = 63, fun width x -> x |> Constant.to_int |> Constant.of_int ~width in
  let binary_string =
    max_bit_width, fun _ x -> x |> Constant.to_binary_string |> Constant.of_binary_string
  in
  let int32 = 32, fun width x -> x |> Constant.to_int32 |> Constant.of_int32 ~width in
  let int64 = 64, fun width x -> x |> Constant.to_int64 |> Constant.of_int64 ~width in
  let hex_string =
    ( max_bit_width
    , fun width x ->
      x
      |> Constant.to_hex_string ~signedness:Unsigned
      |> Constant.of_hex_string ~signedness:Unsigned ~width )
  in
  let z = max_bit_width, fun width x -> x |> Constant.to_z |> Constant.of_z ~width in
  let int64_array =
    ( max_bit_width
    , fun width x -> x |> Constant.to_int64_array |> Constant.of_int64_array ~width )
  in
  let bit_list =
    max_bit_width, fun _ x -> x |> Constant.to_bit_list |> Constant.of_bit_list
  in
  test_round_trip int;
  test_round_trip binary_string;
  test_round_trip int32;
  test_round_trip int64;
  test_round_trip hex_string;
  test_round_trip z;
  test_round_trip int64_array;
  test_round_trip bit_list
;;

let%expect_test "int_of_bstr" =
  let int_of_bstr s = Constant.of_binary_string s |> Constant.to_int in
  [%sexp
    ([ int_of_bstr "0"; int_of_bstr "1"; int_of_bstr "110"; int_of_bstr "11111111" ]
     : int list)]
  |> print_s;
  [%expect {| (0 1 6 255) |}]
;;

let%expect_test "int32_of_bstr" =
  let int32_of_bstr s = Constant.of_binary_string s |> Constant.to_int32 in
  [%sexp
    ([ int32_of_bstr "0"
     ; int32_of_bstr "1"
     ; int32_of_bstr "110"
     ; int32_of_bstr "11111111"
     ]
     : int32 list)]
  |> print_s;
  [%expect {| (0 1 6 255) |}]
;;

let%expect_test "int64_of_bstr" =
  let int64_of_bstr s = Constant.of_binary_string s |> Constant.to_int64 in
  [%sexp
    ([ int64_of_bstr "0"
     ; int64_of_bstr "1"
     ; int64_of_bstr "110"
     ; int64_of_bstr "11111111"
     ]
     : int64 list)]
  |> print_s;
  [%expect {| (0 1 6 255) |}]
;;

let%expect_test "bstr_of_int" =
  let bstr_of_int width x = Constant.of_int ~width x |> Constant.to_binary_string in
  [%sexp
    ([ bstr_of_int 1 0; bstr_of_int 2 1; bstr_of_int 3 6; bstr_of_int 8 255 ]
     : string list)]
  |> print_s;
  [%expect {| (0 01 110 11111111) |}]
;;

let%expect_test "bstr_of_int32" =
  let bstr_of_int32 width x = Constant.of_int32 ~width x |> Constant.to_binary_string in
  [%sexp
    ([ bstr_of_int32 1 0l; bstr_of_int32 2 1l; bstr_of_int32 3 6l; bstr_of_int32 8 255l ]
     : string list)]
  |> print_s;
  [%expect {| (0 01 110 11111111) |}]
;;

let%expect_test "bstr_of_int64" =
  let bstr_of_int64 width x = Constant.of_int64 ~width x |> Constant.to_binary_string in
  [%sexp
    ([ bstr_of_int64 1 0L; bstr_of_int64 2 1L; bstr_of_int64 3 6L; bstr_of_int64 8 255L ]
     : string list)]
  |> print_s;
  [%expect {| (0 01 110 11111111) |}]
;;

let%expect_test "intbitslist_of_bstr" =
  let intbitslist_of_bstr x = Constant.of_binary_string x |> Constant.to_bit_list in
  [%sexp
    (List.map ~f:intbitslist_of_bstr [ "0"; "1"; "1100"; "0011"; "101011101010011" ]
     : int list list)]
  |> print_s;
  [%expect
    {|
    ((0)
     (1)
     (1 1 0 0)
     (0 0 1 1)
     (1 0 1 0 1 1 1 0 1 0 1 0 0 1 1)) |}]
;;

let%expect_test "bstr_of_intbitslist" =
  let bstr_of_intbitslist x = Constant.of_bit_list x |> Constant.to_binary_string in
  [%sexp
    (List.map
       ~f:bstr_of_intbitslist
       [ [ 0 ]
       ; [ 1 ]
       ; [ 1; 1; 0; 0 ]
       ; [ 0; 0; 1; 1 ]
       ; [ 1; 0; 1; 0; 1; 1; 1; 0; 1; 0; 1; 0; 0; 1; 1 ]
       ]
     : string list)]
  |> print_s;
  [%expect {|
    (0 1 1100 0011 101011101010011) |}]
;;

let%expect_test "int_of_hstr" =
  let int_of_hstr s =
    Constant.of_hex_string ~signedness:Unsigned ~width:Int.num_bits s |> Constant.to_int
  in
  [%sexp (List.map ~f:int_of_hstr [ "1"; "B"; "1f3"; "FFFFFFFFFFFFFFFF" ] : int list)]
  |> print_s;
  [%expect {| (1 11 499 -1) |}]
;;

let bstr_of_hstr signedness width x =
  Constant.of_hex_string ~signedness ~width x |> Constant.to_binary_string
;;

let%expect_test "bstr_of_hstr signed" =
  [%sexp
    (List.map ~f:(bstr_of_hstr Signed 64) [ "1"; "B"; "1f3"; "FFFFFFFFFFFFFFFF" ]
     : string list)]
  |> print_s;
  [%expect
    {|
    (0000000000000000000000000000000000000000000000000000000000000001
     1111111111111111111111111111111111111111111111111111111111111011
     0000000000000000000000000000000000000000000000000000000111110011
     1111111111111111111111111111111111111111111111111111111111111111) |}]
;;

let%expect_test "bstr_of_hstr unsigned" =
  [%sexp
    (List.map ~f:(bstr_of_hstr Unsigned 64) [ "1"; "B"; "1f3"; "FFFFFFFFFFFFFFFF" ]
     : string list)]
  |> print_s;
  [%expect
    {|
    (0000000000000000000000000000000000000000000000000000000000000001
     0000000000000000000000000000000000000000000000000000000000001011
     0000000000000000000000000000000000000000000000000000000111110011
     1111111111111111111111111111111111111111111111111111111111111111) |}]
;;

let hstr_of_bstr signedness x =
  Constant.of_binary_string x |> Constant.to_hex_string ~signedness
;;

let%expect_test "hstr_of_bstr signed" =
  [%sexp
    (List.map
       ~f:(hstr_of_bstr Signed)
       [ "1"; "110101"; "00101100100101"; "1111111111111111" ]
     : string list)]
  |> print_s;
  [%expect {| (f f5 0b25 ffff) |}]
;;

let%expect_test "hstr_of_bstr unsigned" =
  [%sexp
    (List.map
       ~f:(hstr_of_bstr Unsigned)
       [ "1"; "110101"; "00101100100101"; "1111111111111111" ]
     : string list)]
  |> print_s;
  [%expect {| (1 35 0b25 ffff) |}]
;;

let%expect_test "bstr_of_abits_int64" =
  let bstr_of_abits_int64 width a =
    Constant.of_int64_array ~width a |> Constant.to_binary_string
  in
  [%sexp
    (List.map
       ~f:(fun (w, a) -> bstr_of_abits_int64 w a)
       [ 3, [| 0x1L |]
       ; 5, [| 0x2L |]
       ; 10, [| 0x3FFL |]
       ; 10, [| 0x4FFL |]
       ; 65, [| 0L; 1L |]
       ; 64, [| 0xfedcba9876543210L |]
       ]
     : string list)]
  |> print_s;
  [%expect
    {|
    (001
     00010
     1111111111
     0011111111
     10000000000000000000000000000000000000000000000000000000000000000
     1111111011011100101110101001100001110110010101000011001000010000) |}]
;;

let%expect_test "abits_int64_of_bstr" =
  let abits_int64_of_bstr s = Constant.of_binary_string s |> Constant.to_int64_array in
  [%sexp
    (List.map
       ~f:abits_int64_of_bstr
       [ "001"
       ; "00010"
       ; "1111111111"
       ; "0011111111"
       ; "10000000000000000000000000000000000000000000000000000000000000000"
       ; "1111111011011100101110101001100001110110010101000011001000010000"
       ]
     : int64 array list)]
  |> print_s;
  [%expect
    {|
    ((1)
     (2)
     (1023)
     (255)
     (0 1)
     (-81985529216486896)) |}]
;;

let%expect_test "[Bits.address_bits_for]" =
  require_does_raise [%here] (fun () -> Bits.address_bits_for (-1));
  [%expect {| ("arg to [address_bits_for] must be >= 0" (got -1)) |}];
  print_s [%message "" ~_:(List.init 10 ~f:Bits.address_bits_for : int list)];
  [%expect {| (1 1 1 2 2 3 3 3 3 4) |}]
;;

let%expect_test "[Bits.num_bits_to_represent]" =
  require_does_raise [%here] (fun () -> Bits.num_bits_to_represent (-1));
  [%expect {| ("arg to [num_bits_to_represent] must be >= 0" (got -1)) |}];
  print_s [%message "" ~_:(List.init 10 ~f:Bits.num_bits_to_represent : int list)];
  [%expect {| (1 1 2 2 3 3 3 3 4 4) |}]
;;
