open Base
open Expect_test_helpers_base
open Hardcaml
open Bits
open With_zero_width

let%expect_test "of_non_zero_width" =
  require_does_raise (fun () -> of_non_zero_width empty);
  [%expect {| "[With_zero_width.of_non_zero_width] empty is not allowed" |}];
  let x = of_non_zero_width vdd in
  print_s [%message (x : t)];
  [%expect {| (x (1)) |}]
;;

let%expect_test "to_non_zero_width" =
  require_does_raise (fun () -> to_non_zero_width None);
  [%expect {| "[With_zero_width.to_non_zero_width] cannot convert 0 width" |}];
  let x = to_non_zero_width (Some vdd) in
  print_s [%message (x : non_zero_width)];
  [%expect {| (x 1) |}]
;;

let%expect_test "constants" =
  print_s [%message (zero_width : t)];
  [%expect {| (zero_width ()) |}];
  require_does_raise (fun () : t -> zero (-1));
  require_does_raise (fun () : t -> one (-1));
  require_does_raise (fun () : t -> ones (-1));
  [%expect
    {|
    ("[zero] requires width greater than or equal to 0" (requested_width -1))
    ("[one] requires width greater than or equal to 0" (requested_width -1))
    ("[ones] requires width greater than or equal to 0" (requested_width -1))
    |}];
  for width = 0 to 2 do
    let zero = zero width in
    let one = one width in
    let ones = ones width in
    print_s [%message (width : int) (zero : t) (one : t) (ones : t)]
  done;
  [%expect
    {|
    ((width 0)
     (zero ())
     (one  ())
     (ones ()))
    ((width 1)
     (zero (0))
     (one  (1))
     (ones (1)))
    ((width 2)
     (zero (00))
     (one  (01))
     (ones (11)))
    |}]
;;

let%expect_test "concat" =
  print_s [%message (concat_lsb [] : t) (concat_msb [] : t)];
  [%expect
    {|
    (("concat_lsb []" ())
     ("concat_msb []" ()))
    |}];
  print_s
    [%message (concat_lsb [ zero_width ] : t) (concat_msb [ zero_width; zero_width ] : t)];
  [%expect
    {|
    (("concat_lsb [zero_width]"             ())
     ("concat_msb [zero_width; zero_width]" ()))
    |}];
  print_s
    [%message
      (concat_lsb [ Some vdd; zero_width ] : t) (concat_msb [ zero_width; Some gnd ] : t)];
  [%expect
    {|
    (("concat_lsb [Some vdd; zero_width]" (1))
     ("concat_msb [zero_width; Some gnd]" (0)))
    |}]
;;

let%expect_test "select" =
  for width = 0 to 2 do
    Stdio.printf "WIDTH: %i\n" width;
    let x = ones width in
    let select h l =
      try Ok (select x ~high:h ~low:l) with
      | e -> Or_error.error_s [%message (e : exn)]
    in
    for lo = -1 to width do
      for hi = lo - 1 to width do
        let result = [%sexp_of: t Or_error.t] (select hi lo) in
        Stdio.printf "[%2i:%2i] %s\n" hi lo (Sexp.to_string result)
      done
    done;
    Stdio.printf "\n"
  done;
  [%expect
    {|
    WIDTH: 0
    [-2:-1] (Error(e("Cannot select a non-zero width into zero width signal"(hi -2)(lo -1))))
    [-1:-1] (Error(e("Cannot select a non-zero width into zero width signal"(hi -1)(lo -1))))
    [ 0:-1] (Error(e("Cannot select a non-zero width into zero width signal"(hi 0)(lo -1))))
    [-1: 0] (Error(e("Cannot select a non-zero width into zero width signal"(hi -1)(lo 0))))
    [ 0: 0] (Error(e("Cannot select a non-zero width into zero width signal"(hi 0)(lo 0))))

    WIDTH: 1
    [-2:-1] (Error(e("[With_zero_width.select] indices are out of bound"(hi -2)(lo -1))))
    [-1:-1] (Error(e("[select] indices are out of bounds"(input_width 1)(hi -1)(lo -1))))
    [ 0:-1] (Error(e("[select] indices are out of bounds"(input_width 1)(hi 0)(lo -1))))
    [ 1:-1] (Error(e("[select] indices are out of bounds"(input_width 1)(hi 1)(lo -1))))
    [-1: 0] (Ok())
    [ 0: 0] (Ok(1))
    [ 1: 0] (Error(e("[select] indices are out of bounds"(input_width 1)(hi 1)(lo 0))))
    [ 0: 1] (Ok())
    [ 1: 1] (Error(e("[select] indices are out of bounds"(input_width 1)(hi 1)(lo 1))))

    WIDTH: 2
    [-2:-1] (Error(e("[With_zero_width.select] indices are out of bound"(hi -2)(lo -1))))
    [-1:-1] (Error(e("[select] indices are out of bounds"(input_width 2)(hi -1)(lo -1))))
    [ 0:-1] (Error(e("[select] indices are out of bounds"(input_width 2)(hi 0)(lo -1))))
    [ 1:-1] (Error(e("[select] indices are out of bounds"(input_width 2)(hi 1)(lo -1))))
    [ 2:-1] (Error(e("[select] indices are out of bounds"(input_width 2)(hi 2)(lo -1))))
    [-1: 0] (Ok())
    [ 0: 0] (Ok(1))
    [ 1: 0] (Ok(11))
    [ 2: 0] (Error(e("[select] indices are out of bounds"(input_width 2)(hi 2)(lo 0))))
    [ 0: 1] (Ok())
    [ 1: 1] (Ok(1))
    [ 2: 1] (Error(e("[select] indices are out of bounds"(input_width 2)(hi 2)(lo 1))))
    [ 1: 2] (Ok())
    [ 2: 2] (Error(e("[select] indices are out of bounds"(input_width 2)(hi 2)(lo 2))))
    |}]
;;

let%expect_test "lsbs" =
  require_does_raise (fun () : t -> lsbs zero_width);
  [%expect {| "cannot take lsbs of zero width signal" |}];
  print_s [%message (lsbs (Some vdd) : t)];
  print_s [%message (lsbs (Some (of_string "00")) : t)];
  [%expect
    {|
    ("lsbs (Some vdd)" ())
    ("lsbs (Some (of_string \"00\"))" (0))
    |}]
;;

let%expect_test "msbs" =
  require_does_raise (fun () : t -> msbs zero_width);
  [%expect {| "cannot take msbs of zero width signal" |}];
  print_s [%message (msbs (Some vdd) : t)];
  print_s [%message (msbs (Some (of_string "00")) : t)];
  [%expect
    {|
    ("msbs (Some vdd)" ())
    ("msbs (Some (of_string \"00\"))" (0))
    |}]
;;

let%expect_test "drop_bottom" =
  print_s [%message (drop_bottom zero_width ~width:0 : t)];
  require_does_raise (fun () -> drop_bottom zero_width ~width:1);
  [%expect
    {|
    ("drop_bottom zero_width ~width:0" ())
    ("Cannot [drop_bottom] non-zero bits from a zero width signal" (n 1))
    |}];
  print_s [%message (drop_bottom (Some vdd) ~width:0 : t)];
  print_s [%message (drop_bottom (Some vdd) ~width:1 : t)];
  require_does_raise (fun () -> drop_bottom (Some vdd) ~width:2);
  [%expect
    {|
    ("drop_bottom (Some vdd) ~width:0" (1))
    ("drop_bottom (Some vdd) ~width:1" ())
    ("[select] got [hi < lo]"
      (hi 0)
      (lo 2))
    |}]
;;

let%expect_test "sel_bottom" =
  print_s [%message (sel_bottom zero_width ~width:0 : t)];
  require_does_raise (fun () -> sel_bottom zero_width ~width:1);
  [%expect
    {|
    ("sel_bottom zero_width ~width:0" ())
    ("Cannot [sel_bottom] non-zero bits from a zero width signal" (n 1))
    |}];
  print_s [%message (sel_bottom (Some vdd) ~width:0 : t)];
  print_s [%message (sel_bottom (Some vdd) ~width:1 : t)];
  require_does_raise (fun () -> sel_bottom (Some vdd) ~width:2);
  [%expect
    {|
    ("sel_bottom (Some vdd) ~width:0" ())
    ("sel_bottom (Some vdd) ~width:1" (1))
    ("[select] indices are out of bounds"
      (input_width 1)
      (hi          1)
      (lo          0))
    |}]
;;

let%expect_test "drop_top" =
  print_s [%message (drop_top zero_width ~width:0 : t)];
  require_does_raise (fun () -> drop_top zero_width ~width:1);
  [%expect
    {|
    ("drop_top zero_width ~width:0" ())
    ("Cannot [drop_top] non-zero bits from a zero width signal" (n 1))
    |}];
  print_s [%message (drop_top (Some vdd) ~width:0 : t)];
  print_s [%message (drop_top (Some vdd) ~width:1 : t)];
  require_does_raise (fun () -> drop_top (Some vdd) ~width:2);
  [%expect
    {|
    ("drop_top (Some vdd) ~width:0" (1))
    ("drop_top (Some vdd) ~width:1" ())
    ("[select] got [hi < lo]"
      (hi -2)
      (lo 0))
    |}]
;;

let%expect_test "sel_top" =
  print_s [%message (sel_top zero_width ~width:0 : t)];
  require_does_raise (fun () -> sel_top zero_width ~width:1);
  [%expect
    {|
    ("sel_top zero_width ~width:0" ())
    ("Cannot [sel_top] non-zero bits from a zero width signal" (n 1))
    |}];
  print_s [%message (sel_top (Some vdd) ~width:0 : t)];
  print_s [%message (sel_top (Some vdd) ~width:1 : t)];
  require_does_raise (fun () -> sel_top (Some vdd) ~width:2);
  [%expect
    {|
    ("sel_top (Some vdd) ~width:0" ())
    ("sel_top (Some vdd) ~width:1" (1))
    ("[select] indices are out of bounds"
      (input_width 1)
      (hi          0)
      (lo          -1))
    |}]
;;

let%expect_test "repeat" =
  print_s [%message (repeat None ~count:0 : t)];
  [%expect {| ("repeat None ~count:0" ()) |}];
  print_s [%message (repeat None ~count:1 : t)];
  [%expect {| ("repeat None ~count:1" ()) |}];
  print_s [%message (repeat (Some vdd) ~count:0 : t)];
  [%expect {| ("repeat (Some vdd) ~count:0" ()) |}];
  print_s [%message (repeat (Some vdd) ~count:1 : t)];
  [%expect {| ("repeat (Some vdd) ~count:1" (1)) |}]
;;

let%expect_test "mux" =
  require_does_raise (fun () -> ignore (mux None [] : non_zero_width));
  [%expect {| "[mux] got no data inputs" |}];
  print_s [%message (mux None [ vdd ] : non_zero_width)];
  [%expect {| ("mux None [vdd]" 1) |}];
  require_does_raise (fun () -> ignore (mux None [ vdd; gnd ] : non_zero_width));
  [%expect {| "[With_zero_width.mux] select is 0 width but there is more than 1 input" |}]
;;
