open! Import
open! Float2

let error a b = Float.(abs (a - b))

let test_range ~start ~stop ~steps ~f_cordic ~f =
  let delta = (stop -. start) /. Float.of_int steps in
  Array.init (steps+1) ~f:(fun i ->
    let arg = start +. (Float.of_int i *. delta) in
    arg, error (f_cordic arg) (f arg))

let%expect_test "cos x, converges when |x| < 1.74" =
  let errors =
    test_range ~start:(-. 2.) ~stop:2. ~steps:20
      ~f_cordic:(fun x -> Cordic_reference.cos_sin x ~iterations:16 |> fst)
      ~f:Float.cos
  in
  print_s [%message "" (errors : (float * log10) array)];
  [%expect {|
    (errors (
      (-2.  1E-0.61)
      (-1.8 1E-1.3)
      (-1.6 1E-4.9)
      (-1.4 1E-4.6)
      (-1.2 1E-4.8)
      (-1.  1E-5.)
      (-0.8 1E-6.1)
      (-0.6 1E-5.)
      (-0.4 1E-5.9)
      (-0.2 1E-5.9)
      (0.   1E-9.8)
      (0.2  1E-5.9)
      (0.4  1E-5.9)
      (0.6  1E-5.)
      (0.8  1E-6.1)
      (1.   1E-5.)
      (1.2  1E-4.8)
      (1.4  1E-4.6)
      (1.6  1E-4.9)
      (1.8  1E-1.3)
      (2.   1E-0.61))) |}]

let%expect_test "sin x, converges when |x| < 1.74" =
  let errors =
    test_range ~start:(-. 2.) ~stop:2. ~steps:20
      ~f_cordic:(fun x -> Cordic_reference.cos_sin ~iterations:16 x |> snd)
      ~f:Float.sin
  in
  print_s [%message "" (errors : (float * log10) array)];
  [%expect {|
    (errors (
      (-2.  1E-1.1)
      (-1.8 1E-1.9)
      (-1.6 1E-6.4)
      (-1.4 1E-5.4)
      (-1.2 1E-5.2)
      (-1.  1E-5.2)
      (-0.8 1E-6.1)
      (-0.6 1E-4.8)
      (-0.4 1E-5.5)
      (-0.2 1E-5.2)
      (0.   1E-4.8)
      (0.2  1E-5.2)
      (0.4  1E-5.5)
      (0.6  1E-4.8)
      (0.8  1E-6.1)
      (1.   1E-5.2)
      (1.2  1E-5.2)
      (1.4  1E-5.4)
      (1.6  1E-6.4)
      (1.8  1E-1.9)
      (2.   1E-1.1))) |}]
;;

let%expect_test "accuracy of cos for different numbers of iterations" =
  let errors =
    Array.init 26 ~f:(fun iterations ->
      let arg = 0.76 in
      let cos, _ = Cordic_reference.cos_sin ~iterations arg in
      let error = error (Float.cos arg) cos in
      iterations, error)
  in
  print_s [%message ""  (errors : (int  * log10) array)];
  [%expect {|
    (errors (
      (0  1E-0.56)
      (1  1E-1.8)
      (2  1E-0.65)
      (3  1E-0.93)
      (4  1E-1.3)
      (5  1E-2.4)
      (6  1E-1.8)
      (7  1E-2.2)
      (8  1E-3.)
      (9  1E-2.7)
      (10 1E-3.4)
      (11 1E-3.6)
      (12 1E-4.)
      (13 1E-4.2)
      (14 1E-4.6)
      (15 1E-4.8)
      (16 1E-5.4)
      (17 1E-5.2)
      (18 1E-5.8)
      (19 1E-5.9)
      (20 1E-6.7)
      (21 1E-6.3)
      (22 1E-6.8)
      (23 1E-7.7)
      (24 1E-7.2)
      (25 1E-7.7))) |}]
;;

let%expect_test "cosh x, converges when |x| < 1.12" =
  let errors =
    test_range ~start:(-. 2.) ~stop:2. ~steps:20
      ~f_cordic:(fun x -> Cordic_reference.cosh_sinh ~iterations:16 x |> fst)
      ~f:Float.cosh
  in
  print_s [%message "" (errors : (float * log10) array)];
  [%expect {|
    (errors (
      (-2.  1E0.32)
      (-1.8 1E0.15)
      (-1.6 1E-0.053)
      (-1.4 1E-0.34)
      (-1.2 1E-0.93)
      (-1.  1E-4.3)
      (-0.8 1E-5.4)
      (-0.6 1E-5.3)
      (-0.4 1E-4.7)
      (-0.2 1E-5.)
      (0.   1E-9.3)
      (0.2  1E-5.)
      (0.4  1E-4.7)
      (0.6  1E-5.3)
      (0.8  1E-5.4)
      (1.   1E-4.3)
      (1.2  1E-0.93)
      (1.4  1E-0.34)
      (1.6  1E-0.053)
      (1.8  1E0.15)
      (2.   1E0.32))) |}]
;;

let%expect_test "sinh x, converges when |x| < 1.12" =
  let errors =
    test_range ~start:(-. 2.) ~stop:2. ~steps:20
      ~f_cordic:(fun x -> Cordic_reference.cosh_sinh ~iterations:16 x |> snd)
      ~f:Float.sinh
  in
  print_s [%message "" (errors : (float * log10) array)];
  [%expect {|
    (errors (
      (-2.  1E0.35)
      (-1.8 1E0.2)
      (-1.6 1E0.0041)
      (-1.4 1E-0.27)
      (-1.2 1E-0.84)
      (-1.  1E-4.1)
      (-0.8 1E-5.2)
      (-0.6 1E-5.)
      (-0.4 1E-4.3)
      (-0.2 1E-4.3)
      (0.   1E-4.5)
      (0.2  1E-4.3)
      (0.4  1E-4.3)
      (0.6  1E-5.)
      (0.8  1E-5.2)
      (1.   1E-4.1)
      (1.2  1E-0.84)
      (1.4  1E-0.27)
      (1.6  1E0.0041)
      (1.8  1E0.2)
      (2.   1E0.35))) |}]
;;

let%expect_test "atan x - no particular convergence requirements" =
  let errors =
    test_range ~start:(-. 10.) ~stop:10. ~steps:20
      ~f_cordic:(fun x -> Cordic_reference.atan ~iterations:16 x)
      ~f:Float.atan
  in
  print_s [%message "" (errors : (float * log10) array)];
  [%expect {|
    (errors (
      (-10. 1E-4.7)
      (-9.  1E-4.8)
      (-8.  1E-4.9)
      (-7.  1E-5.2)
      (-6.  1E-5.)
      (-5.  1E-4.9)
      (-4.  1E-5.)
      (-3.  1E-4.7)
      (-2.  1E-4.9)
      (-1.  1E-4.8)
      (0.   1E-4.8)
      (1.   1E-4.8)
      (2.   1E-4.9)
      (3.   1E-4.7)
      (4.   1E-5.)
      (5.   1E-4.9)
      (6.   1E-5.)
      (7.   1E-5.2)
      (8.   1E-4.9)
      (9.   1E-4.8)
      (10.  1E-4.7))) |}]
;;

let%expect_test "atanh x, converges when |x| < 0.82" =
  (* (1/2) * log2 ((1+t) / (1-t)) *)
  let atanh t =
    0.5 *. (Float.log ((1.0 +. t) /. (1.0 -. t))) in
  let errors =
    test_range ~start:(-. 1.) ~stop:1. ~steps:20
      ~f_cordic:(fun x -> Cordic_reference.atanh ~iterations:16 x)
      ~f:atanh
  in
  print_s [%message "" (errors : (float * log10) array)];
  [%expect {|
    (errors (
      (-1.  1Einf)
      (-0.9 1E-0.45)
      (-0.8 1E-4.5)
      (-0.7 1E-5.5)
      (-0.6 1E-4.6)
      (-0.5 1E-5.2)
      (-0.4 1E-6.5)
      (-0.3 1E-4.3)
      (-0.2 1E-6.5)
      (-0.1 1E-4.2)
      (0.   1E-4.5)
      (0.1  1E-4.2)
      (0.2  1E-6.5)
      (0.3  1E-4.3)
      (0.4  1E-6.5)
      (0.5  1E-5.2)
      (0.6  1E-4.6)
      (0.7  1E-5.5)
      (0.8  1E-4.5)
      (0.9  1E-0.45)
      (1.   1Einf))) |}]
;;

let%expect_test "asin x, converges when |x| < 0.99, but not terribly stable." =
  let errors =
    (* Increase [steps] to 100 to see some poor convergence around [0.66] -
       this appears to be a known issue requiring a specific double iteration
       scheme to circumvent. *)
    test_range ~start:(-. 1.) ~stop:1. ~steps:20
      ~f_cordic:(fun x -> Cordic_reference.asin ~iterations:16 x)
      ~f:Float.asin
  in
  print_s [%message "" (errors : (float * log10) array)];
  [%expect {|
    (errors (
      (-1.  1E-0.76)
      (-0.9 1E-4.6)
      (-0.8 1E-4.8)
      (-0.7 1E-5.2)
      (-0.6 1E-4.8)
      (-0.5 1E-4.8)
      (-0.4 1E-4.9)
      (-0.3 1E-5.)
      (-0.2 1E-4.6)
      (-0.1 1E-5.1)
      (0.   1E-4.8)
      (0.1  1E-5.1)
      (0.2  1E-4.6)
      (0.3  1E-5.)
      (0.4  1E-4.9)
      (0.5  1E-4.8)
      (0.6  1E-4.8)
      (0.7  1E-5.2)
      (0.8  1E-4.8)
      (0.9  1E-4.6)
      (1.   1E-0.76))) |}]
;;

let error_2 (a,b) (c,d) = error a c, error b d

let test_range_2' ~start ~stop ~steps ~f_cordic ~f ~error =
  let delta = (stop -. start) /. Float.of_int steps in
  Array.init (steps+1) ~f:(fun i ->
    let arg1 = start +. (Float.of_int i *. delta) in
    Array.init (steps+1) ~f:(fun j ->
      let arg2 = start +. (Float.of_int j *. delta) in
      arg1, arg2, error (f_cordic arg1 arg2) (f arg1 arg2)))

let test_range_2_2 ~start ~stop ~steps ~f_cordic ~f =
  test_range_2' ~start ~stop ~steps ~f_cordic ~f ~error:error_2

let test_range_2_1 ~start ~stop ~steps ~f_cordic ~f =
  test_range_2' ~start ~stop ~steps ~f_cordic ~f ~error:error

let%expect_test "atan2, no particular convergence range" =
  let errors =
    test_range_2_1 ~start:(-2.) ~stop:2. ~steps:4
      ~f_cordic:(Cordic_reference.atan2 ~iterations:16) ~f:Float.atan2
  in
  print_s [%message "" (errors : (float * float * log10) array array)];
  [%expect {|
    (errors (
      ((-2. -2. 1E-0.21)
       (-2. -1. 1E-0.54)
       (-2. 0.  1E-4.8)
       (-2. 1.  1E-4.9)
       (-2. 2.  1E-4.8))
      ((-1. -2. 1E-0.029)
       (-1. -1. 1E-0.21)
       (-1. 0.  1E-4.8)
       (-1. 1.  1E-4.8)
       (-1. 2.  1E-4.9))
      ((0. -2. 1E0.15)
       (0. -1. 1E0.15)
       (0. 0.  1E0.24)
       (0. 1.  1E-4.8)
       (0. 2.  1E-4.8))
      ((1. -2. 1E-0.029)
       (1. -1. 1E-0.21)
       (1. 0.  1E-4.8)
       (1. 1.  1E-4.8)
       (1. 2.  1E-4.9))
      ((2. -2. 1E-0.21)
       (2. -1. 1E-0.54)
       (2. 0.  1E-4.8)
       (2. 1.  1E-4.9)
       (2. 2.  1E-4.8)))) |}]

module Rect0 = struct
  type t =
    { x : float
    ; y : float }
  [@@deriving sexp_of]
end

module Polar = struct
  type t =
    { magnitude : float
    ; phase : float }
  [@@deriving sexp_of]

  let to_rect { magnitude; phase } : Rect0.t =
    { x = magnitude *. Float.cos phase
    ; y = magnitude *. Float.sin phase }
end

type polar = Polar.t =
  { magnitude : float
  ; phase : float }

module Rect = struct
  include Rect0

  let to_polar { x; y } =
    { magnitude = Float.(sqrt ((x * x) + (y * y)))
    ; phase = Float.(atan (y / x)) }
end

let%expect_test "rect to polar" =
  let errors =
    test_range_2_2 ~start:(0.) ~stop:2. ~steps:4
      ~f_cordic:(Cordic_reference.rect_to_polar ~iterations:16)
      ~f:(fun x y ->
        let { magnitude; phase } = Rect.to_polar { x; y } in
        magnitude, phase)
  in
  print_s [%message "" (errors : (float * float * (log10 * log10)) array array)];
  [%expect {|
    (errors (
      ((0. 0.  (1E-inf 1Enan))
       (0. 0.5 (1E-10. 1E-4.8))
       (0. 1.  (1E-9.8 1E-4.8))
       (0. 1.5 (1E-9.6 1E-4.8))
       (0. 2.  (1E-9.5 1E-4.8)))
      ((0.5 0.  (1E-10. 1E-4.8))
       (0.5 0.5 (1E-10. 1E-4.8))
       (0.5 1.  (1E-10. 1E-4.9))
       (0.5 1.5 (1E-9.5 1E-4.7))
       (0.5 2.  (1E-10. 1E-5.)))
      ((1. 0.  (1E-9.8 1E-4.8))
       (1. 0.5 (1E-10. 1E-4.9))
       (1. 1.  (1E-9.8 1E-4.8))
       (1. 1.5 (1E-11. 1E-5.2))
       (1. 2.  (1E-9.8 1E-4.9)))
      ((1.5 0.  (1E-9.6 1E-4.8))
       (1.5 0.5 (1E-9.5 1E-4.7))
       (1.5 1.  (1E-11. 1E-5.2))
       (1.5 1.5 (1E-9.6 1E-4.8))
       (1.5 2.  (1E-9.4 1E-4.8)))
      ((2. 0.  (1E-9.5 1E-4.8))
       (2. 0.5 (1E-10. 1E-5.))
       (2. 1.  (1E-9.8 1E-4.9))
       (2. 1.5 (1E-9.4 1E-4.8))
       (2. 2.  (1E-9.5 1E-4.8))))) |}]
;;

let%expect_test "polar to rect" =
  let errors =
    test_range_2_2 ~start:(0.) ~stop:2. ~steps:4
      ~f_cordic:(Cordic_reference.polar_to_rect ~iterations:16)
      ~f:(fun magnitude phase ->
        let { Rect. x; y } = Polar.to_rect { magnitude; phase } in
        x, y)
  in
  print_s [%message "" (errors : (float * float * (log10 * log10)) array array)];
  [%expect {|
    (errors (
      ((0. 0.  (1E-inf 1E-inf))
       (0. 0.5 (1E-inf 1E-inf))
       (0. 1.  (1E-inf 1E-inf))
       (0. 1.5 (1E-inf 1E-inf))
       (0. 2.  (1E-inf 1E-inf)))
      ((0.5 0.  (1E-10.  1E-5.1))
       (0.5 0.5 (1E-5.6  1E-5.3))
       (0.5 1.  (1E-5.3  1E-5.5))
       (0.5 1.5 (1E-4.9  1E-6.))
       (0.5 2.  (1E-0.91 1E-1.4)))
      ((1. 0.  (1E-9.8  1E-4.8))
       (1. 0.5 (1E-5.3  1E-5.))
       (1. 1.  (1E-5.   1E-5.2))
       (1. 1.5 (1E-4.6  1E-5.7))
       (1. 2.  (1E-0.61 1E-1.1)))
      ((1.5 0.  (1E-9.6  1E-4.6))
       (1.5 0.5 (1E-5.1  1E-4.9))
       (1.5 1.  (1E-4.9  1E-5.))
       (1.5 1.5 (1E-4.4  1E-5.5))
       (1.5 2.  (1E-0.44 1E-0.94)))
      ((2. 0.  (1E-9.5  1E-4.5))
       (2. 0.5 (1E-5.   1E-4.7))
       (2. 1.  (1E-4.7  1E-4.9))
       (2. 1.5 (1E-4.3  1E-5.4))
       (2. 2.  (1E-0.31 1E-0.82))))) |}]
;;

(* [|a| < 3.4], [|b| < 2.] *)
let%expect_test "mul" =
  let mul a b =
    error (Cordic_reference.mul ~iterations:16 a b) (a *. b)
    |> Float.log10
  in
  print_s [%message "" (mul 0.3 0.5 : float)];
  print_s [%message "" (mul (-0.3) 0.5 : float)];
  print_s [%message "" (mul 2.3 1.9 : float)];
  print_s [%message "" (mul (-3.2) 1.9 : float)];
  print_s [%message "" (mul (-3.2) (-1.9) : float)];
  [%expect {|
    ("mul 0.3 0.5" -5.)
    ("mul (-0.3) 0.5" -5.)
    ("mul 2.3 1.9" -4.9)
    ("mul (-3.2) 1.9" -4.7)
    ("mul (-3.2) (-1.9)" -4.7) |}];
  (* [b] > 2. doesn't converge. *)
  print_s [%message "" (mul 1.3 2.1 : float)];
  [%expect {| ("mul 1.3 2.1" -0.89) |}]
;;

(* [x / y] where [y > 0] and [0.5 x < y]. *)
let%expect_test "div" =
  let div a b =
    error (Cordic_reference.div ~iterations:16 a b) (a /. b)
    |> Float.log10
  in
  print_s [%message "" (div 0.3 0.5 : float)];
  print_s [%message "" (div (-0.3) 0.5 : float)];
  print_s [%message "" (div 1.8 0.9 : float)];
  [%expect {|
    ("div 0.3 0.5" -5.2)
    ("div (-0.3) 0.5" -5.2)
    ("div 1.8 0.9" -4.5) |}];
  (* doesn't converge. *)
  print_s [%message "" (div 1.9 0.9 : float)];
  [%expect {| ("div 1.9 0.9" -0.95) |}];
;;
