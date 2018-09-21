open! Import
open! Float2

module Rect = Test_cordic_reference.Rect
module Polar = Test_cordic_reference.Polar

module Cordic =
  Cordic_special_functions.Make
    (struct
      let width = 32
      let fractional_width = 16
    end)

open Cordic

let test_cos_sin architecture doit =
  let iterations = 16 in
  let sim =
    Cos_sin.Sim.create
      (Cos_sin.create { architecture; iterations })
  in
  let angle = 0.4 in
  let { Cos_sin.Results. cos; sin } = doit ~iterations ~sim { Cos_sin.Args. angle } in
  print_s [%message "" (angle : float) (cos : float) (sin : float)];
;;

let%expect_test "combinational" =
  test_cos_sin Combinational
    (fun ~iterations:_ ~sim arg -> Cos_sin.Test.combinational ~sim arg);
  [%expect {|
    ((angle 0.4)
     (cos   0.92)
     (sin   0.39)) |}]
;;

let%expect_test "pipelined" =
  test_cos_sin Pipelined Cos_sin.Test.pipelined;
  [%expect {|
    ((angle 0.4)
     (cos   0.92)
     (sin   0.39)) |}]
;;

let%expect_test "iterative" =
  test_cos_sin Pipelined Cos_sin.Test.iterative;
  [%expect {|
    ((angle 0.4)
     (cos   0.92)
     (sin   0.39)) |}]
;;

module Test (Function : Cordic_special_functions.Function) = struct
  module Args    = Function.Args
  module Results = Function.Results

  let test args =
    let iterations = 16 in
    let create_sim architecture =
      Function.Sim.create
        (Function.create { architecture; iterations })
    in
    let sim = create_sim Combinational in
    let comb_results = Function.Test.combinational ~sim args in
    let sim = create_sim Iterative in
    let iter_results = Function.Test.iterative ~iterations ~sim args in
    require_equal [%here]
      (module struct
        type t = float Results.t [@@deriving compare, sexp_of]
        let equal = [%compare.equal: t]
      end)
      comb_results iter_results ;
    print_s [%message
      ""
        ~_:(args : float Args.t)
        ~_:(iter_results : float Results.t)]
end

let%expect_test "[Polar_to_rect]" =
  let module Test = Test (Polar_to_rect) in
  Test.test { magnitude = 1.; phase = Float.pi /. 4. };
  [%expect {|
    (((magnitude 1.)   (phase 0.79))
     ((x         0.71) (y     0.71))) |}];
  Test.test { magnitude = 2.; phase = Float.pi /. 2. };
  [%expect {|
    (((magnitude 2.) (phase 1.6))
     ((x         0.) (y     2.))) |}];
;;

let%expect_test "Atan" =
  let module Test = Test (Atan) in
  let test arg =
    Test.test { arg };
    let atan = Float.atan arg in
    print_s [%message "" (atan : float)]
  in
  test 0.4;
  [%expect {|
    (((arg   0.4))
     ((angle 0.38)))
    (atan 0.38) |}];
  test 0.9;
  [%expect {|
    (((arg   0.9))
     ((angle 0.73)))
    (atan 0.73) |}]
;;

let%expect_test "Atan2" =
  let module Test = Test (Atan2) in
  let test x y =
    Test.test { x; y };
    let atan = Float.atan2 x y in
    print_s [%message "" (atan : float)]
  in
  test 0.4 0.5;
  [%expect {|
    (((x 0.4)
      (y 0.5))
     ((angle 0.67)))
    (atan 0.67) |}];
  test 1.3 2.2;
  [%expect {|
    (((x 1.3)
      (y 2.2))
     ((angle 0.53)))
    (atan 0.53) |}]
;;

let%expect_test "Rect_to_polar" =
  let module Test = Test (Rect_to_polar) in
  let test x y =
    Test.test { x; y };
    let polar = Rect.to_polar { x; y } in
    print_s [%message "" ~_:(polar : Polar.t)]
  in
  test 1. 0.;
  [%expect {|
    (((x         1.) (y     0.))
     ((magnitude 1.) (phase -1.5e-05)))
    ((magnitude 1.)
     (phase     0.)) |}];
  test 0.7 0.7;
  [%expect {|
    (((x         0.7)  (y     0.7))
     ((magnitude 0.99) (phase 0.79)))
    ((magnitude 0.99)
     (phase     0.79)) |}];
;;

let%expect_test "Mul" =
  let module Test = Test (Mul) in
  let test a b =
    Test.test { a; b };
    let mul = a *. b in
    print_s [%message "" (mul : float)]
  in
  test 0.5 0.5;
  [%expect {|
    (((a 0.5)
      (b 0.5))
     ((product 0.25)))
    (mul 0.25) |}]
;;

let%expect_test "Div" =
  let module Test = Test (Div) in
  let test a b =
    Test.test { a; b };
    let div = a /. b in
    print_s [%message "" (div : float)]
  in
  test 0.21 0.47;
  [%expect {|
    (((a 0.21)
      (b 0.47))
     ((quotient 0.45)))
    (div 0.45) |}]
;;

let%expect_test "Cosh_sinh" =
  let module Test = Test (Cosh_sinh) in
  let test angle =
    Test.test { angle };
    let cosh, sinh = Float.cosh angle, Float.sinh angle in
    print_s [%message "" (cosh : float) (sinh : float)]
  in
  test 0.3;
  [%expect {|
    (((angle 0.3))
     ((cosh 1.)
      (sinh 0.3)))
    ((cosh 1.)
     (sinh 0.3)) |}];
  test 0.7;
  [%expect {|
    (((angle 0.7))
     ((cosh 1.3)
      (sinh 0.76)))
    ((cosh 1.3)
     (sinh 0.76)) |}]
;;

let%expect_test "Atanh" =
  let module Test = Test (Atanh) in
  let atanh t = 0.5 *. (Float.log ((1.0 +. t) /. (1.0 -. t))) in
  let test arg =
    Test.test { arg };
    let atanh = atanh arg in
    print_s [%message "" (atanh: float)]
  in
  test 0.8;
  [%expect {|
    (((arg   0.8))
     ((angle 1.1)))
    (atanh 1.1) |}];
  test 0.5;
  [%expect {|
    (((arg   0.5))
     ((angle 0.55)))
    (atanh 0.55) |}];
  test 0.1;
  [%expect {|
    (((arg   0.1))
     ((angle 0.1)))
    (atanh 0.1) |}]
;;

let%expect_test "Rotate_vector" =
  let module Test = Test (Rotate_vector) in
  let rotate_vector x y angle =
    (x *. Float.cos angle) -. (y *. Float.sin angle),
    (x *. Float.sin angle) +. (y *. Float.cos angle)
  in
  let test x y angle =
    Test.test { x; y; angle };
    let x, y = rotate_vector x y angle in
    print_s [%message "" (x : float) (y : float)]
  in
  test 1. 0. (Float.pi /. 4.);
  [%expect {|
    (((x     1.)
      (y     0.)
      (angle 0.79))
     ((xo 0.71)
      (yo 0.71)))
    ((x 0.71)
     (y 0.71)) |}];
  test 0.707 0.707 (Float.pi /. 4.);
  [%expect {|
    (((x     0.71)
      (y     0.71)
      (angle 0.79))
     ((xo -1.5e-05)
      (yo 1.)))
    ((x 1.1e-16)
     (y 1.)) |}];
  test 0.33 0.89 0.87;
  [%expect {|
    (((x     0.33)
      (y     0.89)
      (angle 0.87))
     ((xo -0.47)
      (yo 0.83)))
    ((x -0.47)
     (y 0.83)) |}]
;;
