open! Import
open Signal
open Test_constant_propagation.Trace

let sexp_of_signal = Test_constants.sexp_of_const_signal

let%expect_test "[mux]" =
  let sel = of_int ~width:2 2 in
  let width = 3 in
  for length = 0 to 5 do
    print_s
      [%message
        ""
          (length : int)
          ~_:
            (try_with (fun () ->
               mux sel (List.init length ~f:(fun i -> of_int ~width i)))
             : t Or_error.t)]
  done;
  [%expect
    {|
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
       (maximum_expected 4)))) |}]
;;

let%expect_test "mux" =
  let data4 = List.map ~f:(of_int ~width:5) [ 0; 10; 20; 30 ] in
  let mux = fn2 mux in
  print_s
    [%message
      "mux"
        ~_:
          (List.init 4 ~f:(fun i -> mux (of_int ~width:2 i) data4)
           : (signal, signal list) fn2 list)];
  [%expect
    {|
    (mux (
      ((2'b00 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b00000)
      ((2'b01 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b01010)
      ((2'b10 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b10100)
      ((2'b11 (5'b00000 5'b01010 5'b10100 5'b11110)) = 5'b11110))) |}]
;;

let%expect_test "mux2" =
  let mux2 = fn3 mux2 in
  print_s
    [%message
      "check all combinations with mux2"
        ~_:
          (op1 1 ~f:(fun s -> op1 1 ~f:(fun t -> op1 1 ~f:(fun f -> mux2 s t f)))
           : (signal, signal, signal) fn3 list list list)];
  [%expect
    {|
    ("check all combinations with mux2" (
      ((((1'b0 1'b0 1'b0) = 1'b0)
        ((1'b0 1'b0 1'b1) = 1'b1))
       (((1'b0 1'b1 1'b0) = 1'b0)
        ((1'b0 1'b1 1'b1) = 1'b1)))
      ((((1'b1 1'b0 1'b0) = 1'b0)
        ((1'b1 1'b0 1'b1) = 1'b0))
       (((1'b1 1'b1 1'b0) = 1'b1)
        ((1'b1 1'b1 1'b1) = 1'b1))))) |}]
;;
