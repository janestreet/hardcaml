open! Import

type 'a t =
  { a : 'a [@bits 2]
  ; b : 'a [@bits 8]
  ; c : 'a [@bits 3]
  }
[@@deriving sexp_of, hardcaml]

let%expect_test "address offsets" =
  let x = scan port_widths ~init:0 ~f:(fun acc width -> acc + width, acc) in
  print_s [%message (x : int t)];
  [%expect {|
    (x (
      (a 0)
      (b 2)
      (c 10))) |}]
;;

let%expect_test "scan2" =
  let x =
    scan2 port_names port_widths ~init:0 ~f:(fun acc name width ->
      acc + width, (name, acc))
  in
  print_s [%message (x : (string * int) t)];
  [%expect {|
    (x (
      (a (a 0))
      (b (b 2))
      (c (c 10)))) |}]
;;
