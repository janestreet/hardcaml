open! Import

module Config = struct
  let mode = Rac.Mode.Fixed
  let accumulator_bits = 20
  let data_bits = 8
  let num_coefs = 4
  let rom_shift = 6
end

include Test_integer_rac.Make(Config)

let eval ~coefs ~data_in =
  Array.map2_exn coefs data_in ~f:( *. ) |> Array.reduce_exn ~f:(+.)

let test_fixed ?print () ~coefs ~data_in ~coef_prec ~data_prec =
  let ref_result = eval ~coefs ~data_in in
  let rnd f = Float.to_int (if Float.compare f 0.0 < 0 then f -. 0.5 else f +. 0.5) in
  let scale_and_rnd prec f = rnd (f *. Float.of_int (1 lsl prec)) in
  let coefs = Array.map coefs ~f:(scale_and_rnd coef_prec) in
  let data_in = Array.map data_in ~f:(scale_and_rnd data_prec) in
  let result = test ?print () ~coefs ~data_in in
  let scale_result =
    Float.of_int (1 lsl Config.(coef_prec + 1 + data_prec + rom_shift - data_bits)) in
  let float_result = Float.of_int result /. scale_result in
  print_s [%message ""
                      ~precision:(coef_prec, data_prec : int * int)
                      (result : int)
                      (ref_result : float)
                      (float_result : float)]

let%expect_test "precisions" =
  let coefs   = Array.init 4 ~f:(fun _ -> Random.float 1.) in
  let data_in = Array.init 4 ~f:(fun _ -> Random.float 1.) in
  print_s [%message "" (coefs : float array) (data_in : float array)];
  [%expect {|
    ((coefs (
       0.43630556398799153
       0.96035328719918678
       0.56883568253605243
       0.45062527388924589))
     (data_in (
       0.64030838064885942
       0.14207889800896825
       0.26565242651667725
       0.11359872617230203))) |}];
  test_fixed () ~coefs ~data_in ~coef_prec:3 ~data_prec:3;
  [%expect {|
    ((precision (3 3))
     (result       18)
     (ref_result   0.61811908233962443)
     (float_result 0.5625)) |}];
  test_fixed () ~coefs ~data_in ~coef_prec:5 ~data_prec:5;
  [%expect {|
    ((precision (5 5))
     (result       326)
     (ref_result   0.61811908233962443)
     (float_result 0.63671875)) |}];
  test_fixed () ~coefs ~data_in ~coef_prec:7 ~data_prec:7;
  [%expect {|
    ((precision (7 7))
     (result       5079)
     (ref_result   0.61811908233962443)
     (float_result 0.6199951171875)) |}];
  test_fixed () ~coefs ~data_in ~coef_prec:3 ~data_prec:5;
  [%expect {|
    ((precision (3 5))
     (result       80)
     (ref_result   0.61811908233962443)
     (float_result 0.625)) |}];
  test_fixed () ~coefs ~data_in ~coef_prec:5 ~data_prec:3;
  [%expect {|
    ((precision (5 3))
     (result       75)
     (ref_result   0.61811908233962443)
     (float_result 0.5859375)) |}];
;;

let%expect_test "signed" =
  let coefs   = Array.init 4 ~f:(fun _ -> 2. *. (Random.float 1. -. 0.5)) in
  let data_in = Array.init 4 ~f:(fun _ -> 2. *. (Random.float 1. -. 0.5)) in
  print_s [%message "" (coefs : float array) (data_in : float array)];
  [%expect {|
    ((coefs (
       -0.12738887202401694
       0.92070657439837356
       0.13767136507210487
       -0.098749452221508216))
     (data_in (
       0.28061676129771884
       -0.71584220398206355
       -0.46869514696664549
       -0.77280254765539591))) |}];
  test_fixed () ~coefs ~data_in ~coef_prec:6 ~data_prec:6;
  [%expect {|
    ((precision (6 6))
     (result       -1417)
     (ref_result   -0.68304014856006923)
     (float_result -0.69189453125)) |}];
;;

let%expect_test "utilization" =
  let utilization =
    Circuit.create_exn ~name:"rac"
      (Rac.create ~coefs:(Array.init 4 ~f:(Fn.const (Bits.one 8))))
    |> Circuit_utilization.create in
  print_s [%message (utilization : Circuit_utilization.t)];
  [%expect {|
    (utilization (
      (name rac)
      (adders (
        (count             1)
        (total_bits        20)
        (max_instance_bits 20)))
      (subtractors (
        (count             1)
        (total_bits        20)
        (max_instance_bits 20)))
      (multiplexers (
        (count      7)
        (total_bits 208)
        (multiplexers (
          ((number_of_data_elements 2)
           (max_instance_bits       40)
           (total_bits              144)
           (count                   6))
          ((number_of_data_elements 16)
           (max_instance_bits       16)
           (total_bits              64)
           (count                   1))))))
      (registers     ((count 5)  (total_bits 52)))
      (constants     ((count 32) (total_bits 198)))
      (wires         ((count 24) (total_bits 146)))
      (concatenation ((count 12) (total_bits 126)))
      (part_selects  ((count 12) (total_bits 67))))) |}];
;;
