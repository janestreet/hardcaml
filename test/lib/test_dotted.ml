open Core
open Expect_test_helpers_base
open Hardcaml.Bits

let%expect_test "unsigned range" =
  require_does_raise (fun () -> zero 4 +:. -1);
  [%expect
    {|
    ("Failed to perform unsigned integer conversion on dotted operator"
     (op_name add)
     (conversion_error ("[of_unsigned_int] input value is less than 0" (x -1))))
    |}];
  require_does_raise (fun () -> zero 4 -:. 16);
  [%expect
    {|
    ("Failed to perform unsigned integer conversion on dotted operator"
     (op_name sub)
     (conversion_error (
       "[of_unsigned_int] input value is too large for given width"
       (width     4)
       (max_value 15)
       (x         16))))
    |}];
  require_does_not_raise (fun () -> ignore (zero 4 ==:. 0 : t));
  require_does_not_raise (fun () -> ignore (zero 4 >:. 15 : t))
;;

let%expect_test "signed range" =
  require_does_raise (fun () -> zero 4 ++. -9);
  [%expect
    {|
    ("Failed to perform signed integer conversion on dotted operator"
     (op_name add)
     (conversion_error (
       "[of_signed_int] input value is too small for given width"
       (width     4)
       (min_value -8)
       (x         -9))))
    |}];
  require_does_raise (fun () -> zero 4 -+. 8);
  [%expect
    {|
    ("Failed to perform signed integer conversion on dotted operator"
     (op_name sub)
     (conversion_error (
       "[of_signed_int] input value is too large for given width"
       (width     4)
       (max_value 7)
       (x         8))))
    |}];
  require_does_not_raise (fun () -> ignore (zero 4 ==+. -8 : t));
  require_does_not_raise (fun () -> ignore (zero 4 >=+. 7 : t));
  [%expect {| |}]
;;

let test_comparison ~at ~width ~op ~of_int ~expect =
  let expect = of_string expect in
  [%test_result: t] ~expect:expect.:(0) (op (of_int ~width at) (at - 1));
  [%test_result: t] ~expect:expect.:(1) (op (of_int ~width at) at);
  [%test_result: t] ~expect:expect.:(2) (op (of_int ~width at) (at + 1))
;;

let%test_unit "comparison" =
  test_comparison ~width:10 ~at:1 ~op:( <:. ) ~of_int:of_unsigned_int ~expect:"100";
  test_comparison ~width:10 ~at:411 ~op:( <:. ) ~of_int:of_unsigned_int ~expect:"100";
  test_comparison ~width:10 ~at:1022 ~op:( <:. ) ~of_int:of_unsigned_int ~expect:"100";
  test_comparison ~width:10 ~at:(-511) ~op:( <+. ) ~of_int:of_signed_int ~expect:"100";
  test_comparison ~width:10 ~at:0 ~op:( <+. ) ~of_int:of_signed_int ~expect:"100";
  test_comparison ~width:10 ~at:510 ~op:( <+. ) ~of_int:of_signed_int ~expect:"100";
  test_comparison ~width:10 ~at:1 ~op:( <=:. ) ~of_int:of_unsigned_int ~expect:"110";
  test_comparison ~width:10 ~at:411 ~op:( <=:. ) ~of_int:of_unsigned_int ~expect:"110";
  test_comparison ~width:10 ~at:1022 ~op:( <=:. ) ~of_int:of_unsigned_int ~expect:"110";
  test_comparison ~width:10 ~at:(-511) ~op:( <=+. ) ~of_int:of_signed_int ~expect:"110";
  test_comparison ~width:10 ~at:0 ~op:( <=+. ) ~of_int:of_signed_int ~expect:"110";
  test_comparison ~width:10 ~at:510 ~op:( <=+. ) ~of_int:of_signed_int ~expect:"110";
  test_comparison ~width:10 ~at:1 ~op:( >:. ) ~of_int:of_unsigned_int ~expect:"001";
  test_comparison ~width:10 ~at:411 ~op:( >:. ) ~of_int:of_unsigned_int ~expect:"001";
  test_comparison ~width:10 ~at:1022 ~op:( >:. ) ~of_int:of_unsigned_int ~expect:"001";
  test_comparison ~width:10 ~at:(-511) ~op:( >+. ) ~of_int:of_signed_int ~expect:"001";
  test_comparison ~width:10 ~at:0 ~op:( >+. ) ~of_int:of_signed_int ~expect:"001";
  test_comparison ~width:10 ~at:510 ~op:( >+. ) ~of_int:of_signed_int ~expect:"001";
  test_comparison ~width:10 ~at:1 ~op:( >=:. ) ~of_int:of_unsigned_int ~expect:"011";
  test_comparison ~width:10 ~at:411 ~op:( >=:. ) ~of_int:of_unsigned_int ~expect:"011";
  test_comparison ~width:10 ~at:1022 ~op:( >=:. ) ~of_int:of_unsigned_int ~expect:"011";
  test_comparison ~width:10 ~at:(-511) ~op:( >=+. ) ~of_int:of_signed_int ~expect:"011";
  test_comparison ~width:10 ~at:0 ~op:( >=+. ) ~of_int:of_signed_int ~expect:"011";
  test_comparison ~width:10 ~at:510 ~op:( >=+. ) ~of_int:of_signed_int ~expect:"011"
;;

let%test_unit "equality" =
  test_comparison ~width:6 ~at:1 ~op:( ==:. ) ~of_int:of_unsigned_int ~expect:"010";
  test_comparison ~width:6 ~at:19 ~op:( ==:. ) ~of_int:of_unsigned_int ~expect:"010";
  test_comparison ~width:6 ~at:62 ~op:( ==:. ) ~of_int:of_unsigned_int ~expect:"010";
  test_comparison ~width:6 ~at:(-31) ~op:( ==+. ) ~of_int:of_signed_int ~expect:"010";
  test_comparison ~width:6 ~at:0 ~op:( ==+. ) ~of_int:of_signed_int ~expect:"010";
  test_comparison ~width:6 ~at:30 ~op:( ==+. ) ~of_int:of_signed_int ~expect:"010";
  test_comparison ~width:6 ~at:1 ~op:( <>:. ) ~of_int:of_unsigned_int ~expect:"101";
  test_comparison ~width:6 ~at:19 ~op:( <>:. ) ~of_int:of_unsigned_int ~expect:"101";
  test_comparison ~width:6 ~at:62 ~op:( <>:. ) ~of_int:of_unsigned_int ~expect:"101";
  test_comparison ~width:6 ~at:(-31) ~op:( <>+. ) ~of_int:of_signed_int ~expect:"101";
  test_comparison ~width:6 ~at:0 ~op:( <>+. ) ~of_int:of_signed_int ~expect:"101";
  test_comparison ~width:6 ~at:30 ~op:( <>+. ) ~of_int:of_signed_int ~expect:"101"
;;

let%test_unit "arithmetic" =
  let test ~of_int swop hwop ~width a b =
    [%test_result: t] ~expect:(of_int_trunc ~width (swop a b)) (hwop (of_int ~width a) b)
  in
  let add = test ~of_int:of_unsigned_int ( + ) ( +:. ) in
  add ~width:4 3 7;
  add ~width:4 15 15;
  add ~width:30 0x1234abe 0x3be13551;
  let add = test ~of_int:of_signed_int ( + ) ( ++. ) in
  add ~width:4 (-2) 6;
  add ~width:4 2 (-7);
  add ~width:4 (-3) (-1);
  let sub = test ~of_int:of_unsigned_int ( - ) ( -:. ) in
  sub ~width:4 3 7;
  sub ~width:4 15 15;
  sub ~width:30 0x1234abe 0x3be13551;
  let sub = test ~of_int:of_signed_int ( - ) ( -+. ) in
  sub ~width:4 (-2) 6;
  sub ~width:4 2 (-7);
  sub ~width:4 (-3) (-1)
;;

let%expect_test "logical" =
  let test ~of_int swop hwop ~width a b =
    [%test_result: t] ~expect:(of_int_trunc ~width (swop a b)) (hwop (of_int ~width a) b)
  in
  let and_ = test ~of_int:of_unsigned_int ( land ) ( &:. ) in
  and_ ~width:20 996_654 31_129;
  and_ ~width:2 2 3;
  let and_ = test ~of_int:of_signed_int ( land ) ( &+. ) in
  and_ ~width:20 (-412_223) (-123);
  and_ ~width:5 15 (-5);
  let or_ = test ~of_int:of_unsigned_int ( lor ) ( |:. ) in
  or_ ~width:20 996_654 31_129;
  or_ ~width:2 2 3;
  let or_ = test ~of_int:of_signed_int ( lor ) ( |+. ) in
  or_ ~width:20 (-412_223) (-123);
  or_ ~width:5 15 (-5);
  let xor_ = test ~of_int:of_unsigned_int ( lxor ) ( ^:. ) in
  xor_ ~width:20 996_654 31_129;
  xor_ ~width:2 2 3;
  let xor_ = test ~of_int:of_signed_int ( lxor ) ( ^+. ) in
  xor_ ~width:20 (-412_223) (-123);
  xor_ ~width:5 15 (-5)
;;
