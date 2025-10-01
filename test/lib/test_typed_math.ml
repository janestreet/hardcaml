open Core
open Hardcaml

let test_op_u bits_a bits_b a b fh fs =
  let h =
    fh (Bits.of_unsigned_int ~width:bits_a a) (Bits.of_unsigned_int ~width:bits_b b)
  in
  let mask = (1 lsl Bits.width h) - 1 in
  let h = Bits.to_unsigned_int h in
  let s = fs a b land mask in
  [%test_eq: int] s h
;;

let test_op_s bits_a bits_b a b fh fs =
  let h =
    fh (Bits.of_signed_int ~width:bits_a a) (Bits.of_signed_int ~width:bits_b b)
    |> Bits.to_signed_int
  in
  let s = fs a b in
  [%test_eq: int] s h
;;

let test_ops bits_a bits_b =
  let max_a = 1 lsl bits_a in
  let max_b = 1 lsl bits_b in
  for a = 0 to max_a - 1 do
    for b = 0 to max_b - 1 do
      let sa = a - (max_a / 2) in
      let sb = b - (max_b / 2) in
      test_op_u bits_a bits_b a b Bits.Unsigned.( +: ) ( + );
      test_op_s bits_a bits_b sa sb Bits.Signed.( +: ) ( + );
      test_op_u bits_a bits_b a b Bits.Unsigned.( -: ) ( - );
      test_op_s bits_a bits_b sa sb Bits.Signed.( -: ) ( - );
      test_op_u bits_a bits_b a b Bits.Unsigned.( *: ) ( * );
      test_op_s bits_a bits_b sa sb Bits.Signed.( *: ) ( * );
      test_op_u bits_a bits_b a b Bits.Unsigned.( <: ) (fun a b -> if a < b then 1 else 0);
      test_op_s bits_a bits_b sa sb Bits.Signed.( <: ) (fun a b ->
        if a < b then -1 else 0);
      test_op_u bits_a bits_b a b Bits.Unsigned.( <=: ) (fun a b ->
        if a <= b then 1 else 0);
      test_op_s bits_a bits_b sa sb Bits.Signed.( <=: ) (fun a b ->
        if a <= b then -1 else 0);
      test_op_u bits_a bits_b a b Bits.Unsigned.( >: ) (fun a b -> if a > b then 1 else 0);
      test_op_s bits_a bits_b sa sb Bits.Signed.( >: ) (fun a b ->
        if a > b then -1 else 0);
      test_op_u bits_a bits_b a b Bits.Unsigned.( >=: ) (fun a b ->
        if a >= b then 1 else 0);
      test_op_s bits_a bits_b sa sb Bits.Signed.( >=: ) (fun a b ->
        if a >= b then -1 else 0);
      test_op_u bits_a bits_b a b Bits.Unsigned.( ==: ) (fun a b ->
        if a = b then 1 else 0);
      test_op_s bits_a bits_b sa sb Bits.Signed.( ==: ) (fun a b ->
        if a = b then -1 else 0);
      test_op_u bits_a bits_b a b Bits.Unsigned.( <>: ) (fun a b ->
        if a <> b then 1 else 0);
      test_op_s bits_a bits_b sa sb Bits.Signed.( <>: ) (fun a b ->
        if a <> b then -1 else 0)
    done
  done
;;

let%expect_test "test ops" =
  test_ops 2 3;
  test_ops 4 1;
  test_ops 5 7
;;

let%expect_test "resize" =
  print_s [%message (Bits.Unsigned.resize (Bits.of_string "1011") 2 : Bits.t)];
  print_s [%message (Bits.Unsigned.resize (Bits.of_string "10") 5 : Bits.t)];
  print_s [%message (Bits.Signed.resize (Bits.of_string "1011") 2 : Bits.t)];
  print_s [%message (Bits.Signed.resize (Bits.of_string "10") 5 : Bits.t)];
  [%expect
    {|
    ("Bits.Unsigned.resize (Bits.of_string \"1011\") 2" 11)
    ("Bits.Unsigned.resize (Bits.of_string \"10\") 5" 00010)
    ("Bits.Signed.resize (Bits.of_string \"1011\") 2" 11)
    ("Bits.Signed.resize (Bits.of_string \"10\") 5" 11110)
    |}]
;;

let%expect_test "truncate" =
  for i = 0 to 7 do
    let r = Bits.Unsigned.truncate (Bits.of_unsigned_int ~width:3 i) ~width:1 in
    print_s [%message "" ~_:(r.valid : Bits.t) ~_:(r.value : Bits.t)]
  done;
  [%expect
    {|
    (1 0)
    (1 1)
    (0 0)
    (0 1)
    (0 0)
    (0 1)
    (0 0)
    (0 1)
    |}];
  for i = 0 to 7 do
    let r = Bits.Unsigned.truncate (Bits.of_unsigned_int ~width:3 i) ~width:2 in
    print_s [%message "" ~_:(r.valid : Bits.t) ~_:(r.value : Bits.t)]
  done;
  [%expect
    {|
    (1 00)
    (1 01)
    (1 10)
    (1 11)
    (0 00)
    (0 01)
    (0 10)
    (0 11)
    |}];
  for i = -4 to 3 do
    let r = Bits.Signed.truncate (Bits.of_signed_int ~width:3 i) ~width:1 in
    print_s [%message "" ~_:(r.valid : Bits.t) ~_:(r.value : Bits.t)]
  done;
  [%expect
    {|
    (0 0)
    (0 1)
    (0 0)
    (1 1)
    (1 0)
    (0 1)
    (0 0)
    (0 1)
    |}];
  for i = -4 to 3 do
    let r = Bits.Signed.truncate (Bits.of_signed_int ~width:3 i) ~width:2 in
    print_s [%message "" ~_:(r.valid : Bits.t) ~_:(r.value : Bits.t)]
  done;
  [%expect
    {|
    (0 00)
    (0 01)
    (1 10)
    (1 11)
    (1 00)
    (1 01)
    (0 10)
    (0 11)
    |}];
  Expect_test_helpers_base.require_does_raise (fun () ->
    Bits.Unsigned.truncate (Bits.of_string "1111") ~width:0);
  Expect_test_helpers_base.require_does_raise (fun () ->
    Bits.Unsigned.truncate (Bits.of_string "1111") ~width:5);
  Expect_test_helpers_base.require_does_raise (fun () ->
    Bits.Signed.truncate (Bits.of_string "1111") ~width:0);
  Expect_test_helpers_base.require_does_raise (fun () ->
    Bits.Signed.truncate (Bits.of_string "1111") ~width:5);
  [%expect
    {|
    ("[Typed_math.truncate] output width must be >= 0" (output_width 0))
    ("[Typed_math.truncate] Output width must be less than or equal to input width"
     (output_width 5)
     (input_width  4))
    ("[Typed_math.truncate] output width must be >= 0" (output_width 0))
    ("[Typed_math.truncate] Output width must be less than or equal to input width"
     (output_width 5)
     (input_width  4))
    |}]
;;
