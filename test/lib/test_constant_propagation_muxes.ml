open! Import
open Signal.Const_prop.Comb
open Test_constant_propagation.Trace

let sexp_of_signal = Test_constants.sexp_of_const_signal

let show signal = print_s [%sexp (signal : signal)]

let%expect_test "[mux]" =
  let sel = consti 2 2 in
  let width = 3 in
  for length = 0 to 5 do
    print_s [%message
      ""
        (length : int)
        ~_:(try_with (fun () ->
          mux sel (List.init length ~f:(fun i -> consti width i))) : t Or_error.t)];
  done;
  [%expect {|
    ((length 0)
     (Error  "[mux] got empty list"))
    ((length 1) (Error ("[mux] got fewer than 2 inputs" (inputs_provided 1))))
    ((length 2)
     (Ok (
       const
       (width 3)
       (value 0b001))))
    ((length 3)
     (Ok (
       const
       (width 3)
       (value 0b010))))
    ((length 4)
     (Ok (
       const
       (width 3)
       (value 0b010))))
    ((length 5)
     (Error (
       "[mux] got too many inputs"
       (inputs_provided  5)
       (maximum_expected 4)))) |}];
;;

let%expect_test "mux" =
  let data4 = List.map ~f:(consti 5) [ 0; 10; 20; 30 ] in
  let mux = fn2 mux in
  print_s
    [%message "mux"
                ~_:(List.init 4 ~f:(fun i -> mux (consti 2 i) data4)
                    : (signal, signal list) fn2 list) ];
  [%expect {|
    (mux (
      ((2'b00 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b00000)
      ((2'b01 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b01010)
      ((2'b10 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b10100)
      ((2'b11 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b11110))) |}]

let%expect_test "mux2" =
  let mux2 = fn3 mux2 in
  print_s
    [%message "check all combinations with mux2"
                ~_:(op1 1 ~f:(fun s -> op1 1 ~f:(fun t -> op1 1 ~f:(fun f -> mux2 s t f)))
                    : (signal, signal, signal) fn3 list list list) ];
  [%expect {|
    ("check all combinations with mux2" (
      ((((1'b0 1'b0 1'b0) = 1'b0)
        ((1'b0 1'b0 1'b1) = 1'b1))
       (((1'b0 1'b1 1'b0) = 1'b0)
        ((1'b0 1'b1 1'b1) = 1'b1)))
      ((((1'b1 1'b0 1'b0) = 1'b0)
        ((1'b1 1'b0 1'b1) = 1'b0))
       (((1'b1 1'b1 1'b0) = 1'b1)
        ((1'b1 1'b1 1'b1) = 1'b1))))) |}]

let test_cases idx =
  let cases = fn3 cases in
  cases (consti 3 idx) (consti 5 11)
    [ 3, consti 5  0
    ; 4, consti 5 10
    ; 6, consti 5 20 ]

let%expect_test "cases default" =
  print_s
    [%message "cases"
                ~_:(List.map ~f:test_cases [0; 3; 4; 6; 7]
                    : (signal, signal, (int*signal) list) fn3 list) ];
  [%expect {|
    (cases (
      ((3'b000 5'b01011 (
         (3 5'b00000)
         (4 5'b01010)
         (6 5'b10100)))
       =
       5'b01011)
      ((3'b011 5'b01011 (
         (3 5'b00000)
         (4 5'b01010)
         (6 5'b10100)))
       =
       5'b00000)
      ((3'b100 5'b01011 (
         (3 5'b00000)
         (4 5'b01010)
         (6 5'b10100)))
       =
       5'b01010)
      ((3'b110 5'b01011 (
         (3 5'b00000)
         (4 5'b01010)
         (6 5'b10100)))
       =
       5'b10100)
      ((3'b111 5'b01011 (
         (3 5'b00000)
         (4 5'b01010)
         (6 5'b10100)))
       =
       5'b01011))) |}]

let%expect_test "[pmux]" =
  let c i = consti 3 i in
  let test cases = show (pmux cases (c 7)) in
  test [];
  [%expect {| 3'b111 |}];
  test [ vdd, c 1 ];
  [%expect {| 3'b001 |}];
  test [ gnd, c 1 ];
  [%expect {| 3'b111 |}];
  test [ gnd, c 1; gnd, c 2 ];
  [%expect {| 3'b111 |}];
  test [ gnd, c 1; vdd, c 2 ];
  [%expect {| 3'b010 |}];
  test [ vdd, c 1; vdd, c 2 ];
  [%expect {| 3'b001 |}];
  test [ vdd, c 1; gnd, c 2 ];
  [%expect {| 3'b001 |}];
;;

let test_pmux idx =
  let pmux = fn2 pmux in
  pmux
    (List.mapi
       ~f:(fun i v -> (if (idx land (1 lsl i)) <> 0 then vdd else gnd), consti 9 v)
       [ 0x64; 0xc8 ])
    (consti 9 0x21)

let%expect_test "pmux" =
  print_s
    [%message "pmux"
                ~_:(List.init 4 ~f:test_pmux : ((signal * signal) list, signal) fn2 list) ];
  [%expect {|
    (pmux (
      ((((1'b0 9'h064)
         (1'b0 9'h0c8))
        9'h021)
       =
       9'h021)
      ((((1'b1 9'h064)
         (1'b0 9'h0c8))
        9'h021)
       =
       9'h064)
      ((((1'b0 9'h064)
         (1'b1 9'h0c8))
        9'h021)
       =
       9'h0c8)
      ((((1'b1 9'h064)
         (1'b1 9'h0c8))
        9'h021)
       =
       9'h064))) |}]

let%expect_test "[pmuxl []]" =
  show_raise (fun () -> pmuxl []);
  [%expect {| (raised "[reduce] got empty list") |}];
;;

let%expect_test "[pmuxl]" =
  let c i = consti 2 i in
  let test cases = show (pmuxl cases) in
  test [ vdd, c 1 ];
  [%expect {| 2'b01 |}];
  test [ gnd, c 1 ];
  [%expect {| 2'b01 |}];
  test [ gnd, c 1; gnd, c 2 ];
  [%expect {| 2'b10 |}];
  test [ gnd, c 1; vdd, c 2 ];
  [%expect {| 2'b10 |}];
  test [ vdd, c 1; vdd, c 2 ];
  [%expect {| 2'b01 |}];
  test [ vdd, c 1; gnd, c 2 ];
  [%expect {| 2'b01 |}];
;;

let test_pmuxl idx =
  (fn1 pmuxl)
    (List.mapi
       ~f:(fun i v -> (if (idx land (1 lsl i)) <> 0 then vdd else gnd), consti 9 v)
       [ 100; 200 ])

let%expect_test "pmuxl" =
  print_s
    [%message "pmuxl"
                ~_:(List.init 4 ~f:test_pmuxl : (signal * signal) list fn1 list) ];
  [%expect {|
    (pmuxl (
      (((1'b0 9'h064) (1'b0 9'h0c8)) = 9'h0c8)
      (((1'b1 9'h064) (1'b0 9'h0c8)) = 9'h064)
      (((1'b0 9'h064) (1'b1 9'h0c8)) = 9'h0c8)
      (((1'b1 9'h064) (1'b1 9'h0c8)) = 9'h064))) |}]

let%expect_test "[pmux1h []]" =
  show_raise (fun () -> pmux1h []);
  [%expect {| (raised "[reduce] got empty list") |}];
;;

let%expect_test "[pmux1h]" =
  let c i = consti 2 i in
  let test cases = show (pmux1h cases) in
  test [ vdd, c 1 ];
  [%expect {| 2'b01 |}];
  test [ gnd, c 1 ];
  [%expect {| 2'b00 |}];
  test [ gnd, c 1; gnd, c 2 ];
  [%expect {| 2'b00 |}];
  test [ gnd, c 1; vdd, c 2 ];
  [%expect {| 2'b10 |}];
  test [ vdd, c 1; vdd, c 2 ];
  [%expect {| 2'b11 |}];
  test [ vdd, c 1; gnd, c 2 ];
  [%expect {| 2'b01 |}];
;;

let test_pmux1h idx =
  (fn1 pmux1h)
    (List.mapi
       ~f:(fun i v -> (if (idx land (1 lsl i)) <> 0 then vdd else gnd), consti 8 v)
       [ 100; 200 ])

let%expect_test "pmux1h" =
  print_s
    [%message "pmux1h"
                ~_:(List.init 4 ~f:test_pmux1h : (signal * signal) list fn1 list) ];
  [%expect {|
    (pmux1h (
      (((1'b0 8'b01100100) (1'b0 8'b11001000)) = 8'b00000000)
      (((1'b1 8'b01100100) (1'b0 8'b11001000)) = 8'b01100100)
      (((1'b0 8'b01100100) (1'b1 8'b11001000)) = 8'b11001000)
      (((1'b1 8'b01100100) (1'b1 8'b11001000)) = 8'b11101100))) |}]

