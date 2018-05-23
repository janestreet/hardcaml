(* test various string conversions in [Utils] *)
open! Import
open Utils

let%expect_test "list_of_string" =
  [%sexp (String.to_list "123abc" : char list)] |> print_s;
  [%expect {| (1 2 3 a b c) |}]

let%expect_test "int_of_hchar" =
  [%sexp (List.map ~f:int_of_hchar @@ String.to_list "0123456789abcdefABCDEF" : int list)]
  |> print_s;
  [%expect {| (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 10 11 12 13 14 15) |}]

let%expect_test "int_of_hchar raises" =
  require_does_raise [%here] (fun () -> int_of_hchar 'g');
  [%expect {| ("Hardcaml__Utils.Failure(\"Invalid hex char\")") |}]

let%expect_test "int_of_bchar" =
  [%sexp (List.map ~f:int_of_bchar @@ String.to_list "01" : int list)] |> print_s;
  [%expect {| (0 1) |}]

let%expect_test "int_of_bchar raises" =
  require_does_raise [%here] (fun () -> int_of_bchar 'g');
  [%expect {| ("Hardcaml__Utils.Failure(\"int_of_bin_char: Invalid binary character encountered\")") |}]

let%expect_test "[int_of_bstr] raise" =
  require_does_raise [%here] ~cr:CR_soon (fun () -> int_of_bstr "");
  [%expect {|
    "did not raise" |}];
  require_does_raise [%here] ~cr:CR_soon (fun () -> int_of_bstr "z");
  [%expect {|
    "did not raise" |}];
;;

let%expect_test "[bstr_of_int*], [int*_of_bstr], [hstr_of_bstr]" =
  let test_round_trip_int round_trip =
    for width = 1 to 10 do
      for i = 0 to 1 lsl width - 1 do
        require_equal [%here] (module Int) i (i |> round_trip width)
      done;
    done in
  test_round_trip_int (fun width x -> x |> bstr_of_int width |> int_of_bstr);
  test_round_trip_int
    (fun width x ->
       x
       |> Int32.of_int_exn
       |> bstr_of_int32 width
       |> int32_of_bstr
       |> Int32.to_int_exn);
  test_round_trip_int
    (fun width x ->
       x
       |> Int64.of_int_exn
       |> bstr_of_int64 width
       |> int64_of_bstr
       |> Int64.to_int_exn);
  test_round_trip_int
    (fun width x ->
       x
       |> bstr_of_int width
       |> hstr_of_bstr Unsigned
       |> int_of_hstr);
;;

let%expect_test "[*_of_bstr] and [bstr_of_*] are inverses" =
  let test_round_trip_bstr round_trip =
    for width = 1 to 10 do
      for i = 0 to 1 lsl width - 1 do
        let bstr = bstr_of_int width i in
        require_equal [%here] (module String) bstr (bstr |> round_trip width)
      done;
    done in
  List.iter [ Signedness. Signed; Unsigned ] ~f:(fun sign ->
    test_round_trip_bstr (fun width x ->
      x |> hstr_of_bstr sign |> bstr_of_hstr sign width));
  test_round_trip_bstr (fun width x ->
    x |> abits_int32_of_bstr |> bstr_of_abits_int32 width);
  test_round_trip_bstr (fun width x ->
    x |> abits_int64_of_bstr |> bstr_of_abits_int64 width);
  test_round_trip_bstr (fun _ x ->
    x |> intbitslist_of_bstr |> bstr_of_intbitslist);
;;

let%expect_test "int_of_bstr" =
  [%sexp
    ([ int_of_bstr "0"
     ; int_of_bstr "1"
     ; int_of_bstr "110"
     ; int_of_bstr "11111111" ] : int list)] |> print_s;
  [%expect {| (0 1 6 255) |}]

let%expect_test "int32_of_bstr" =
  [%sexp
    ([ int32_of_bstr "0"
     ; int32_of_bstr "1"
     ; int32_of_bstr "110"
     ; int32_of_bstr "11111111" ] : int32 list)] |> print_s;
  [%expect {| (0 1 6 255) |}]

let%expect_test "int64_of_bstr" =
  [%sexp
    ([ int64_of_bstr "0"
     ; int64_of_bstr "1"
     ; int64_of_bstr "110"
     ; int64_of_bstr "11111111" ] : int64 list)] |> print_s;
  [%expect {| (0 1 6 255) |}]

let%expect_test "nativeint_of_bstr" =
  [%sexp
    ([ nativeint_of_bstr "0"
     ; nativeint_of_bstr "1"
     ; nativeint_of_bstr "110"
     ; nativeint_of_bstr "11111111" ] : nativeint list)] |> print_s;
  [%expect {| (0 1 6 255) |}]

let%expect_test "bstr_of_int" =
  [%sexp
    ([ bstr_of_int 1 0
     ; bstr_of_int 2 1
     ; bstr_of_int 3 6
     ; bstr_of_int 8 255 ] : string list) ] |> print_s;
  [%expect {| (0 01 110 11111111) |}]

let%expect_test "bstr_of_int32" =
  [%sexp
    ([ bstr_of_int32 1 0l
     ; bstr_of_int32 2 1l
     ; bstr_of_int32 3 6l
     ; bstr_of_int32 8 255l ] : string list) ] |> print_s;
  [%expect {| (0 01 110 11111111) |}]

let%expect_test "bstr_of_int64" =
  [%sexp
    ([ bstr_of_int64 1 0L
     ; bstr_of_int64 2 1L
     ; bstr_of_int64 3 6L
     ; bstr_of_int64 8 255L ] : string list) ] |> print_s;
  [%expect {| (0 01 110 11111111) |}]

let%expect_test "bstr_of_nint" =
  [%sexp
    ([ bstr_of_nint 1 0n
     ; bstr_of_nint 2 1n
     ; bstr_of_nint 3 6n
     ; bstr_of_nint 8 255n ] : string list) ] |> print_s;
  [%expect {| (0 01 110 11111111) |}]

let%expect_test "intbitslist_of_bstr" =
  [%sexp (List.map ~f:intbitslist_of_bstr
            [ "0"
            ; "1"
            ; "1100"
            ; "0011"
            ; "101011101010011" ]
          : int list list)] |> print_s;
  [%expect {|
    ((0)
     (1)
     (1 1 0 0)
     (0 0 1 1)
     (1 0 1 0 1 1 1 0 1 0 1 0 0 1 1)) |}]

let%expect_test "bstr_of_intbitslist" =
  [%sexp (List.map ~f:bstr_of_intbitslist
            [ [0]
            ; [1]
            ; [1;1;0;0]
            ; [0;0;1;1]
            ; [1;0;1;0;1;1;1;0;1;0;1;0;0;1;1] ]
          : string list)] |> print_s;
  [%expect {|
    (0 1 1100 0011 101011101010011) |}]

let%expect_test "int_of_hstr" =
  [%sexp (List.map ~f:int_of_hstr
            [ "1"
            ; "B"
            ; "1f3"
            ; "FFFFFFFFFFFFFFFF" ]
          : int list)] |> print_s;
  [%expect {| (1 11 499 -1) |}]

let%expect_test "bstr_of_hstr signed" =
  [%sexp (List.map ~f:(bstr_of_hstr Signed 64)
            [ "1"
            ; "B"
            ; "1f3"
            ; "FFFFFFFFFFFFFFFF" ]
          : string list)] |> print_s;
  [%expect {|
    (0000000000000000000000000000000000000000000000000000000000000001
     1111111111111111111111111111111111111111111111111111111111111011
     0000000000000000000000000000000000000000000000000000000111110011
     1111111111111111111111111111111111111111111111111111111111111111) |}]

let%expect_test "bstr_of_hstr unsigned" =
  [%sexp (List.map ~f:(bstr_of_hstr Unsigned 64)
            [ "1"
            ; "B"
            ; "1f3"
            ; "FFFFFFFFFFFFFFFF" ]
          : string list)] |> print_s;
  [%expect {|
    (0000000000000000000000000000000000000000000000000000000000000001
     0000000000000000000000000000000000000000000000000000000000001011
     0000000000000000000000000000000000000000000000000000000111110011
     1111111111111111111111111111111111111111111111111111111111111111) |}]

let%expect_test "hstr_of_bstr signed" =
  [%sexp (List.map ~f:(hstr_of_bstr Signed)
            [ "1"
            ; "110101"
            ; "00101100100101"
            ; "1111111111111111" ]
          : string list)] |> print_s;
  [%expect {| (f f5 0b25 ffff) |}]

let%expect_test "hstr_of_bstr unsigned" =
  [%sexp (List.map ~f:(hstr_of_bstr Unsigned)
            [ "1"
            ; "110101"
            ; "00101100100101"
            ; "1111111111111111" ]
          : string list)] |> print_s;
  [%expect {| (1 35 0b25 ffff) |}]

let%expect_test "bstr_of_abits_int32" =
  [%sexp (List.map ~f:(fun (w, a) -> bstr_of_abits_int32 w a)
            [  3, [| 0x1l |]
            ;  5, [| 0x2l |]
            ; 10, [| 0x3FFl |]
            ; 10, [| 0x4FFl |]
            ; 33, [| 0l; 1l |]
            ; 64, [| 0x76543210l; 0xfedcba98l |] ]
          : string list) ] |> print_s;
  [%expect {|
    (001
     00010
     1111111111
     0011111111
     100000000000000000000000000000000
     1111111011011100101110101001100001110110010101000011001000010000) |}]

let%expect_test "bstr_of_abits_int64" =
  [%sexp (List.map ~f:(fun (w, a) -> bstr_of_abits_int64 w a)
            [  3, [| 0x1L |]
            ;  5, [| 0x2L |]
            ; 10, [| 0x3FFL |]
            ; 10, [| 0x4FFL |]
            ; 65, [| 0L; 1L |]
            ; 64, [| 0xfedcba9876543210L |] ]
          : string list) ] |> print_s;
  [%expect {|
    (001
     00010
     1111111111
     0011111111
     10000000000000000000000000000000000000000000000000000000000000000
     1111111011011100101110101001100001110110010101000011001000010000) |}]

let%expect_test "bstr_of_abits_nint" =
  [%sexp (List.map ~f:(fun (w, a) -> bstr_of_abits_nint w a)
            [  3, [| 0x1n |]
            ;  5, [| 0x2n |]
            ; 10, [| 0x3FFn |]
            ; 10, [| 0x4FFn |] ]
          : string list) ] |> print_s;
  [%expect {| (001 00010 1111111111 0011111111) |}]

let%expect_test "abits_int32_of_bstr" =
  [%sexp (List.map ~f:abits_int32_of_bstr
            [ "001"
            ; "00010"
            ; "1111111111"
            ; "0011111111"
            ; "10000000000000000000000000000000000000000000000000000000000000000"
            ; "1111111011011100101110101001100001110110010101000011001000010000" ]
          : int32 array list) ] |> print_s;
  [%expect {|
    ((1)
     (2)
     (1023)
     (255)
     (0 0 1)
     (1985229328 -19088744)) |}]

let%expect_test "abits_int64_of_bstr" =
  [%sexp (List.map ~f:abits_int64_of_bstr
            [ "001"
            ; "00010"
            ; "1111111111"
            ; "0011111111"
            ; "10000000000000000000000000000000000000000000000000000000000000000"
            ; "1111111011011100101110101001100001110110010101000011001000010000" ]
          : int64 array list) ] |> print_s;
  [%expect {|
    ((1)
     (2)
     (1023)
     (255)
     (0 1)
     (-81985529216486896)) |}]

let%expect_test "abits_nint_of_bstr" =
  [%sexp (List.map ~f:abits_nint_of_bstr
            [ "001"
            ; "00010"
            ; "1111111111"
            ; "0011111111" ]
          : nativeint array list) ] |> print_s;
  [%expect {|
    ((1)
     (2)
     (1023)
     (255)) |}]
