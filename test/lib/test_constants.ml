open! Import

open Signal

let sexp_of_const_signal ?(depth = 1) signal =
  let open Signal in
  match signal with
  | Empty -> [%sexp "empty"]
  | _ ->
    if is_const signal
    then (
      if width signal <= 8
      then [%sexp (Int.to_string (width signal) ^ "'b" ^ Signal.to_bstr signal : string)]
      else [%sexp (Int.to_string (width signal)
                   ^ "'h"
                   ^ (Signal.to_constant signal
                      |> Constant.to_hex_string ~signedness:Unsigned)
                   : string)])
    else
      match names signal with
      | []    ->
        let sexp_of_signal = sexp_of_signal_recursive ~depth in
        [%message "Not a constant" (signal : signal) ]
      | [ n ] -> [%sexp (n : string)]
      | ns    -> [%sexp (ns : string list)]

let sexp_of_const_function sexp_of_args (args, const_signal) =
  [%message "" ~_:(args : args) ~_:(const_signal : const_signal)]

let app1 f a = a, f a
let app2 f a b = (a, b), f ~width:a b
let consti   = app2 consti
let consti32 = app2 consti32
let consti64 = app2 consti64
let sexp_of_consti   = sexp_of_const_function [%sexp_of: int * int]
let sexp_of_consti32 = sexp_of_const_function [%sexp_of: int * int32]
let sexp_of_consti64 = sexp_of_const_function [%sexp_of: int * int64]

let%expect_test "width empty = 0" =
  print_s [%message "" ~_:(width empty : int)];
  [%expect {| 0 |}]

let%expect_test "constb" =
  let constb = app1 constb in
  print_s [%message
    "binary string to signal conversion"
      ~constb:(List.map ~f:constb
                 ["0"; "1"; "10"; "111"; "10101"] : string const_function list)];
  [%expect {|
    ("binary string to signal conversion" (
      constb (
        (0     1'b0)
        (1     1'b1)
        (10    2'b10)
        (111   3'b111)
        (10101 5'b10101)))) |}];
  require_does_raise [%here] (fun () -> constb "foo");
  [%expect {| ("[constb] got invalid binary constant" foo) |}]
;;

let%expect_test "consti[xx] 2" =
  print_s [%message
    "integer to signal conversions"
      ~consti:   (List.init 4 ~f:(consti 2) : consti list)
      ~consti32: (List.init 4 ~f:(fun i -> consti32 2 (Int32.of_int_exn i)) : consti32 list)
      ~consti64: (List.init 4 ~f:(fun i -> consti64 2 (Int64.of_int_exn i)) : consti64 list)];
  [%expect {|
    ("integer to signal conversions"
      (consti (
        ((2 0) 2'b00)
        ((2 1) 2'b01)
        ((2 2) 2'b10)
        ((2 3) 2'b11)))
      (consti32 (
        ((2 0) 2'b00)
        ((2 1) 2'b01)
        ((2 2) 2'b10)
        ((2 3) 2'b11)))
      (consti64 (
        ((2 0) 2'b00)
        ((2 1) 2'b01)
        ((2 2) 2'b10)
        ((2 3) 2'b11)))) |}]

let%expect_test "consti[xx] wrap on overflow" =
  print_s [%message
    "integer constant conversion wraps on overflow"
      ~consti:   (consti   3 8  : consti)
      ~consti32: (consti32 3 9l : consti32)
      ~consti64: (consti64 3 10L : consti64) ];
  [%expect {|
    ("integer constant conversion wraps on overflow"
     (consti   ((3 8)  3'b000))
     (consti32 ((3 9)  3'b001))
     (consti64 ((3 10) 3'b010))) |}]

type 'a min_max = { min : 'a; max : 'a }[@@deriving sexp_of]

let min_max (type a) const (module Int : Int.S with type t = a) =
  { min = const Int.num_bits Int.min_value
  ; max = const Int.num_bits Int.max_value }

let%expect_test "minimum and maximum" =
  print_s [%message
    ""
      ~consti:   (min_max consti   (module Int)   : consti   min_max)
      ~consti32: (min_max consti32 (module Int32) : consti32 min_max)
      ~consti64: (min_max consti64 (module Int64) : consti64 min_max) ];
  [%expect {|
    ((consti (
       (min ((63 -4611686018427387904) 63'h4000000000000000))
       (max ((63 4611686018427387903)  63'h3fffffffffffffff))))
     (consti32 (
       (min ((32 -2147483648) 32'h80000000))
       (max ((32 2147483647)  32'h7fffffff))))
     (consti64 (
       (min ((64 -9223372036854775808) 64'h8000000000000000))
       (max ((64 9223372036854775807)  64'h7fffffffffffffff))))) |}]

let%expect_test "consth{u,s}" =
  let consthu = app2 consthu in
  let consths = app2 consths in
  let sexp_of_signed_and_unsigned (signed, unsigned) =
    [%message "consth"
                (signed:   (int * string) const_function)
                (unsigned: (int * string) const_function)] in
  let consth w h = consths w h, consthu w h in
  let hex = "0123456789abcdef" in
  let hexstr len = String.init len ~f:(fun i -> hex.[(len-i-1) % 16]) in
  print_s [%message
    "hex string to signal conversion"
      ~_:(consth 1 "f"            : signed_and_unsigned)
      ~_:(consth 2 "f"            : signed_and_unsigned)
      ~_:(consth 3 "f"            : signed_and_unsigned)
      ~_:(consth 4 "f"            : signed_and_unsigned)
      ~_:(consth 5 "f"            : signed_and_unsigned)
      ~_:(consth 6 "f"            : signed_and_unsigned)
      ~_:(consth 16  @@ hexstr  4 : signed_and_unsigned)
      ~_:(consth 17  @@ hexstr  4 : signed_and_unsigned)
      ~_:(consth 100 @@ hexstr 24 : signed_and_unsigned)
      ~_:(consth 104 @@ hexstr 25 : signed_and_unsigned)];
  [%expect {|
    ("hex string to signal conversion"
      (consth
        (signed   ((1 f) 1'b1))
        (unsigned ((1 f) 1'b1)))
      (consth
        (signed   ((2 f) 2'b11))
        (unsigned ((2 f) 2'b11)))
      (consth
        (signed   ((3 f) 3'b111))
        (unsigned ((3 f) 3'b111)))
      (consth
        (signed   ((4 f) 4'b1111))
        (unsigned ((4 f) 4'b1111)))
      (consth
        (signed   ((5 f) 5'b11111))
        (unsigned ((5 f) 5'b01111)))
      (consth
        (signed   ((6 f) 6'b111111))
        (unsigned ((6 f) 6'b001111)))
      (consth
        (signed   ((16 3210) 16'h3210))
        (unsigned ((16 3210) 16'h3210)))
      (consth
        (signed   ((17 3210) 17'h03210))
        (unsigned ((17 3210) 17'h03210)))
      (consth
        (signed ((100 76543210fedcba9876543210) 100'h076543210fedcba9876543210))
        (unsigned ((100 76543210fedcba9876543210) 100'h076543210fedcba9876543210)))
      (consth
        (signed ((104 876543210fedcba9876543210) 104'hf876543210fedcba9876543210))
        (unsigned (
          (104 876543210fedcba9876543210) 104'h0876543210fedcba9876543210)))) |}]

let%expect_test "constd error" =
  require_does_raise [%here] (fun () -> constd ~width:10 "a");
  [%expect {| ("[constd] got invalid decimal char" a) |}];
  require_does_raise [%here] (fun () -> constd ~width:10 "");
  [%expect {| "[constd] got empty string" |}]
;;

let%expect_test "constv" =
  let constv = app1 constv in
  let const_prop_constv = app1 Signal.Const_prop.Comb.constv in
  print_s [%message
    "verilog style constant conversion"
      ~binary:  ([ constv "1'b0"
                 ; constv "1'b1"
                 ; constv "3'b1"
                 ; constv "10'b1010101010" ] : string const_function list)
      ~decimal: ([ const_prop_constv "16'd65535"
                 ; const_prop_constv "17'd65536" ] : string const_function list)
      ~hex:     ([ constv "5'h4"
                 ; constv "5'h8"
                 ; constv "5'H4"
                 ; constv "5'H8" ] : string const_function list)
      ~decimal_requires_constant_propagates:
        ([ constv "16'd65535"
         ; constv "17'd65536" ] : string const_function list) ];
  [%expect {|
    ("verilog style constant conversion"
      (binary (
        (1'b0           1'b0)
        (1'b1           1'b1)
        (3'b1           3'b001)
        (10'b1010101010 10'h2aa)))
      (decimal (
        (16'd65535 16'hffff)
        (17'd65536 17'h10000)))
      (hex (
        (5'h4 5'b00100)
        (5'h8 5'b01000)
        (5'H4 5'b00100)
        (5'H8 5'b11000)))
      (decimal_requires_constant_propagates (
        (16'd65535 (
          "Not a constant" (
            signal (
              select
              (loc   test_constants.ml:LINE:COL)
              (width 16)
              (range (15 0))
              (data_in add)))))
        (17'd65536 (
          "Not a constant" (
            signal (
              select
              (loc   test_constants.ml:LINE:COL)
              (width 17)
              (range (16 0))
              (data_in add)))))))) |}];
  require_does_raise [%here] (fun () -> constv "2323");
  [%expect {| ("[constv] missing [']" 2323) |}];
  require_does_raise [%here] (fun () -> constv "'");
  [%expect {| ("[constv] missing bit count" ') |}];
  require_does_raise [%here] (fun () -> constv "5'b");
  [%expect {| ("[constv] value shorter than 2 characters" 5'b) |}];
  require_does_raise [%here] (fun () -> constv "5'J1");
  [%expect {| ("[constv] bad control character" (const 5'J1)) |}]
;;

let%expect_test "constibl" =
  let constibl = app1 constibl in
  print_s [%message
    "int bits list to signal conversion"
      ~_:(List.map ~f:constibl
            [ [0]; [1]; [0;1]; [1;1;1] ] : int list const_function list) ];
  [%expect {|
    ("int bits list to signal conversion" (
      ((0) 1'b0)
      ((1) 1'b1)
      ((0 1) 2'b01)
      ((1 1 1) 3'b111))) |}]

let%expect_test "simple constants" =
  let one  = app1 one  in
  let ones = app1 ones in
  let zero = app1 zero in
  print_s [%message
    "one, ones and zero to signal"
      ~one:  (List.init 4 ~f:(fun b -> one  (b+1)) : int const_function list)
      ~ones: (List.init 4 ~f:(fun b -> ones (b+1)) : int const_function list)
      ~zero: (List.init 4 ~f:(fun b -> zero (b+1)) : int const_function list) ];
  [%expect {|
    ("one, ones and zero to signal"
      (one (
        (1 1'b1)
        (2 2'b01)
        (3 3'b001)
        (4 4'b0001)))
      (ones (
        (1 1'b1)
        (2 2'b11)
        (3 3'b111)
        (4 4'b1111)))
      (zero (
        (1 1'b0)
        (2 2'b00)
        (3 3'b000)
        (4 4'b0000)))) |}]
