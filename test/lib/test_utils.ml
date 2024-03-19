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
  [%expect {| "[Constant.of_binary_string] input string is empty" |}];
  require_does_raise [%here] ~cr:CR_soon (fun () -> to_bstr "z");
  [%expect
    {| ("[Constant.of_binary_string] input must only consist of '1' or '0'" (got z)) |}]
;;

let%expect_test "{to,of}_z resepects sign" =
  let max_bit_width = 200 in
  let num_tests = 10_000 in
  let test_z_roundtrip_signed () =
    let width = 1 + Random.int max_bit_width in
    let bits = Bits.random ~width in
    let is_signed = Bits.msb bits |> Bits.to_bool in
    let z = Bits.to_z ~signedness:Signed bits in
    let bits' = Bits.of_z ~width z in
    let failed msg =
      let sexp_of_z z = Zarith.Z.to_string z |> [%sexp_of: string] in
      raise_s [%message msg (bits : Bits.t) (bits' : Bits.t) (z : z)]
    in
    if not (Bits.equal bits bits') then failed "bit vector roundtrip via Z failed";
    if is_signed && Zarith.Z.(compare z zero >= 0)
    then failed "[Z] value was expected to be negative"
  in
  for _ = 1 to num_tests do
    test_z_roundtrip_signed ()
  done
;;

let test_round_trip =
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
  test_round_trip
;;

let%expect_test "roundtrips" =
  let max_bit_width = 200 in
  let int =
    (* Test at 32 bits, which is compatible with 64 bit OCaml and js_of_ocaml *)
    32, fun width x -> x |> Constant.to_int |> Constant.of_int ~width
  in
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
  let z =
    ( max_bit_width
    , fun width x -> x |> Constant.to_z ~signedness:Unsigned |> Constant.of_z ~width )
  in
  let z_signed =
    ( max_bit_width
    , fun width x -> x |> Constant.to_z ~signedness:Signed |> Constant.of_z ~width )
  in
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
  test_round_trip z_signed;
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
     (1 0 1 0 1 1 1 0 1 0 1 0 0 1 1))
    |}]
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
  [%expect {| (0 1 1100 0011 101011101010011) |}]
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
     1111111111111111111111111111111111111111111111111111111111111111)
    |}]
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
     1111111111111111111111111111111111111111111111111111111111111111)
    |}]
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
     1111111011011100101110101001100001110110010101000011001000010000)
    |}]
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
     (-81985529216486896))
    |}]
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

let%expect_test "raw strings" =
  let test str ~width =
    let of_string = Constant.Raw.of_string ~width str |> Bits.of_constant in
    let of_bytes =
      Constant.Raw.of_bytes ~width (Bytes.of_string str) |> Bits.of_constant
    in
    let to_string = Bits.to_constant of_string |> Constant.Raw.to_string in
    let to_bytes = Bits.to_constant of_bytes |> Constant.Raw.to_bytes in
    print_s
      [%message
        (of_string : Bits.t)
          (to_string : String.t)
          (of_bytes : Bits.t)
          (to_bytes : Bytes.t)]
  in
  test "\002" ~width:1;
  test "\002" ~width:2;
  test "\002" ~width:8;
  test "\002" ~width:32;
  [%expect
    {|
    ((of_string 0)
     (to_string "\000")
     (of_bytes  0)
     (to_bytes  "\000"))
    ((of_string 10)
     (to_string "\002")
     (of_bytes  10)
     (to_bytes  "\002"))
    ((of_string 00000010)
     (to_string "\002")
     (of_bytes  00000010)
     (to_bytes  "\002"))
    ((of_string 00000000000000000000000000000010)
     (to_string "\002\000\000\000")
     (of_bytes  00000000000000000000000000000010)
     (to_bytes  "\002\000\000\000"))
    |}];
  test "\001\002\003\004" ~width:8;
  test "\001\002\003\004" ~width:16;
  test "\001\002\003\004" ~width:24;
  test "\001\002\003\004" ~width:32;
  test "\001\002\003\004" ~width:40;
  [%expect
    {|
    ((of_string 00000001)
     (to_string "\001")
     (of_bytes  00000001)
     (to_bytes  "\001"))
    ((of_string 0000001000000001)
     (to_string "\001\002")
     (of_bytes  0000001000000001)
     (to_bytes  "\001\002"))
    ((of_string 000000110000001000000001)
     (to_string "\001\002\003")
     (of_bytes  000000110000001000000001)
     (to_bytes  "\001\002\003"))
    ((of_string 00000100000000110000001000000001)
     (to_string "\001\002\003\004")
     (of_bytes  00000100000000110000001000000001)
     (to_bytes  "\001\002\003\004"))
    ((of_string 0000000000000100000000110000001000000001)
     (to_string "\001\002\003\004\000")
     (of_bytes  0000000000000100000000110000001000000001)
     (to_bytes  "\001\002\003\004\000"))
    |}];
  for width = 8 to 15 do
    test "\000\255" ~width
  done;
  [%expect
    {|
    ((of_string 00000000)
     (to_string "\000")
     (of_bytes  00000000)
     (to_bytes  "\000"))
    ((of_string 100000000)
     (to_string "\000\001")
     (of_bytes  100000000)
     (to_bytes  "\000\001"))
    ((of_string 1100000000)
     (to_string "\000\003")
     (of_bytes  1100000000)
     (to_bytes  "\000\003"))
    ((of_string 11100000000)
     (to_string "\000\007")
     (of_bytes  11100000000)
     (to_bytes  "\000\007"))
    ((of_string 111100000000)
     (to_string "\000\015")
     (of_bytes  111100000000)
     (to_bytes  "\000\015"))
    ((of_string 1111100000000)
     (to_string "\000\031")
     (of_bytes  1111100000000)
     (to_bytes  "\000\031"))
    ((of_string 11111100000000)
     (to_string "\000?")
     (of_bytes  11111100000000)
     (to_bytes  "\000?"))
    ((of_string 111111100000000)
     (to_string "\000\127")
     (of_bytes  111111100000000)
     (to_bytes  "\000\127"))
    |}]
;;
