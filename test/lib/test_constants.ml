open! Import
open Signal

let sexp_of_const_signal ?(depth = 1) signal =
  let open Signal in
  match signal with
  | Empty -> [%sexp "empty"]
  | _ ->
    if is_const signal
    then
      if
        width signal <= 8
      then [%sexp (Int.to_string (width signal) ^ "'b" ^ Signal.to_bstr signal : string)]
      else
        [%sexp
          (Int.to_string (width signal)
           ^ "'h"
           ^ (Signal.to_constant signal |> Constant.to_hex_string ~signedness:Unsigned)
           : string)]
    else (
      match names signal with
      | [] ->
        let sexp_of_signal = sexp_of_signal_recursive ~depth in
        [%message "Not a constant" (signal : signal)]
      | [ n ] -> [%sexp (n : string)]
      | ns -> [%sexp (ns : string list)])
;;

let sexp_of_const_function sexp_of_args (args, const_signal) =
  [%message "" ~_:(args : args) ~_:(const_signal : const_signal)]
;;

let app1 f a = a, f a
let app2 f a b = (a, b), f ~width:a b
let of_int = app2 of_int
let of_int32 = app2 of_int32
let of_int64 = app2 of_int64
let sexp_of_of_int = sexp_of_const_function [%sexp_of: int * int]
let sexp_of_of_int32 = sexp_of_const_function [%sexp_of: int * int32]
let sexp_of_of_int64 = sexp_of_const_function [%sexp_of: int * int64]

let%expect_test "width empty = 0" =
  print_s [%message "" ~_:(width empty : int)];
  [%expect {| 0 |}]
;;

let%expect_test "of_bit_string" =
  let of_bit_string = app1 of_bit_string in
  print_s
    [%message
      "binary string to signal conversion"
        ~of_bit_string:
          (List.map ~f:of_bit_string [ "0"; "1"; "10"; "111"; "10101" ]
           : string const_function list)];
  [%expect
    {|
    ("binary string to signal conversion" (
      of_bit_string (
        (0     1'b0)
        (1     1'b1)
        (10    2'b10)
        (111   3'b111)
        (10101 5'b10101)))) |}];
  require_does_raise [%here] (fun () -> of_bit_string "foo");
  [%expect {| ("[of_bit_string] got invalid binary constant" foo) |}]
;;

let%expect_test "of_int[xx] 2" =
  print_s
    [%message
      "integer to signal conversions"
        ~of_int:(List.init 4 ~f:(of_int 2) : of_int list)
        ~of_int32:
          (List.init 4 ~f:(fun i -> of_int32 2 (Int32.of_int_exn i)) : of_int32 list)
        ~of_int64:
          (List.init 4 ~f:(fun i -> of_int64 2 (Int64.of_int_exn i)) : of_int64 list)];
  [%expect
    {|
    ("integer to signal conversions"
      (of_int (
        ((2 0) 2'b00)
        ((2 1) 2'b01)
        ((2 2) 2'b10)
        ((2 3) 2'b11)))
      (of_int32 (
        ((2 0) 2'b00)
        ((2 1) 2'b01)
        ((2 2) 2'b10)
        ((2 3) 2'b11)))
      (of_int64 (
        ((2 0) 2'b00)
        ((2 1) 2'b01)
        ((2 2) 2'b10)
        ((2 3) 2'b11)))) |}]
;;

let%expect_test "of_int[xx] wrap on overflow" =
  print_s
    [%message
      "integer constant conversion wraps on overflow"
        ~of_int:(of_int 3 8 : of_int)
        ~of_int32:(of_int32 3 9l : of_int32)
        ~of_int64:(of_int64 3 10L : of_int64)];
  [%expect
    {|
    ("integer constant conversion wraps on overflow"
     (of_int   ((3 8)  3'b000))
     (of_int32 ((3 9)  3'b001))
     (of_int64 ((3 10) 3'b010))) |}]
;;

type 'a min_max =
  { min : 'a
  ; max : 'a
  }
[@@deriving sexp_of]

let min_max (type a) const (module Int : Int.S with type t = a) =
  { min = const Int.num_bits Int.min_value; max = const Int.num_bits Int.max_value }
;;

let%expect_test "minimum and maximum" =
  print_s
    [%message
      ""
        ~of_int:(min_max of_int (module Int) : of_int min_max)
        ~of_int32:(min_max of_int32 (module Int32) : of_int32 min_max)
        ~of_int64:(min_max of_int64 (module Int64) : of_int64 min_max)];
  [%expect
    {|
    ((of_int (
       (min ((63 -4611686018427387904) 63'h4000000000000000))
       (max ((63 4611686018427387903)  63'h3fffffffffffffff))))
     (of_int32 (
       (min ((32 -2147483648) 32'h80000000))
       (max ((32 2147483647)  32'h7fffffff))))
     (of_int64 (
       (min ((64 -9223372036854775808) 64'h8000000000000000))
       (max ((64 9223372036854775807)  64'h7fffffffffffffff))))) |}]
;;

let%expect_test "of_hex" =
  let of_hexu = app2 (of_hex ~signedness:Unsigned) in
  let of_hexs = app2 (of_hex ~signedness:Signed) in
  let sexp_of_signed_and_unsigned (signed, unsigned) =
    [%message
      "of_hex"
        (signed : (int * string) const_function)
        (unsigned : (int * string) const_function)]
  in
  let of_hex w h = of_hexs w h, of_hexu w h in
  let hex = "0123456789abcdef" in
  let hexstr len = String.init len ~f:(fun i -> hex.[(len - i - 1) % 16]) in
  print_s
    [%message
      "hex string to signal conversion"
        ~_:(of_hex 1 "f" : signed_and_unsigned)
        ~_:(of_hex 2 "f" : signed_and_unsigned)
        ~_:(of_hex 3 "f" : signed_and_unsigned)
        ~_:(of_hex 4 "f" : signed_and_unsigned)
        ~_:(of_hex 5 "f" : signed_and_unsigned)
        ~_:(of_hex 6 "f" : signed_and_unsigned)
        ~_:(of_hex 16 @@ hexstr 4 : signed_and_unsigned)
        ~_:(of_hex 17 @@ hexstr 4 : signed_and_unsigned)
        ~_:(of_hex 100 @@ hexstr 24 : signed_and_unsigned)
        ~_:(of_hex 104 @@ hexstr 25 : signed_and_unsigned)];
  [%expect
    {|
    ("hex string to signal conversion"
      (of_hex
        (signed   ((1 f) 1'b1))
        (unsigned ((1 f) 1'b1)))
      (of_hex
        (signed   ((2 f) 2'b11))
        (unsigned ((2 f) 2'b11)))
      (of_hex
        (signed   ((3 f) 3'b111))
        (unsigned ((3 f) 3'b111)))
      (of_hex
        (signed   ((4 f) 4'b1111))
        (unsigned ((4 f) 4'b1111)))
      (of_hex
        (signed   ((5 f) 5'b11111))
        (unsigned ((5 f) 5'b01111)))
      (of_hex
        (signed   ((6 f) 6'b111111))
        (unsigned ((6 f) 6'b001111)))
      (of_hex
        (signed   ((16 3210) 16'h3210))
        (unsigned ((16 3210) 16'h3210)))
      (of_hex
        (signed   ((17 3210) 17'h03210))
        (unsigned ((17 3210) 17'h03210)))
      (of_hex
        (signed ((100 76543210fedcba9876543210) 100'h076543210fedcba9876543210))
        (unsigned ((100 76543210fedcba9876543210) 100'h076543210fedcba9876543210)))
      (of_hex
        (signed ((104 876543210fedcba9876543210) 104'hf876543210fedcba9876543210))
        (unsigned (
          (104 876543210fedcba9876543210) 104'h0876543210fedcba9876543210)))) |}]
;;

let%expect_test "of_decimal_string error" =
  require_does_raise [%here] (fun () -> of_decimal_string ~width:10 "a");
  [%expect {| ("[of_decimal_string] got invalid decimal char" a) |}];
  require_does_raise [%here] (fun () -> of_decimal_string ~width:10 "");
  [%expect {| "[of_decimal_string] got empty string" |}]
;;

let%expect_test "of_string" =
  let raw_of_string = app1 Signal.Unoptimized.of_string in
  let of_string = app1 of_string in
  print_s
    [%message
      "verilog style constant conversion"
        ~binary:
          ([ of_string "1'b0"
           ; of_string "1'b1"
           ; of_string "3'b1"
           ; of_string "10'b1010101010"
           ]
           : string const_function list)
        ~decimal:
          ([ of_string "16'd65535"; of_string "17'd65536" ] : string const_function list)
        ~hex:
          ([ of_string "5'h4"; of_string "5'h8"; of_string "5'H4"; of_string "5'H8" ]
           : string const_function list)
        ~decimal_requires_constant_propagates:
          ([ raw_of_string "16'd65535"; raw_of_string "17'd65536" ]
           : string const_function list)];
  [%expect
    {|
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
            signal (select (width 16) (range (15 0)) (data_in add)))))
        (17'd65536 (
          "Not a constant" (
            signal (select (width 17) (range (16 0)) (data_in add)))))))) |}];
  require_does_raise [%here] (fun () -> of_string "2323");
  [%expect {| ("[of_string] could not convert constant" (const 2323)) |}];
  require_does_raise [%here] (fun () -> of_string "'");
  [%expect {| ("[of_string] could not convert constant" (const ')) |}];
  require_does_raise [%here] (fun () -> of_string "5'b");
  [%expect {| ("[of_string] could not convert constant" (const 5'b)) |}];
  require_does_raise [%here] (fun () -> of_string "5'J1");
  [%expect {| ("[of_string] could not convert constant" (const 5'J1)) |}]
;;

let%expect_test "of_bit_list" =
  let of_bit_list = app1 of_bit_list in
  print_s
    [%message
      "int bits list to signal conversion"
        ~_:
          (List.map ~f:of_bit_list [ [ 0 ]; [ 1 ]; [ 0; 1 ]; [ 1; 1; 1 ] ]
           : int list const_function list)];
  [%expect
    {|
    ("int bits list to signal conversion" (
      ((0) 1'b0)
      ((1) 1'b1)
      ((0 1) 2'b01)
      ((1 1 1) 3'b111))) |}]
;;

let%expect_test "simple constants" =
  let one = app1 one in
  let ones = app1 ones in
  let zero = app1 zero in
  print_s
    [%message
      "one, ones and zero to signal"
        ~one:(List.init 4 ~f:(fun b -> one (b + 1)) : int const_function list)
        ~ones:(List.init 4 ~f:(fun b -> ones (b + 1)) : int const_function list)
        ~zero:(List.init 4 ~f:(fun b -> zero (b + 1)) : int const_function list)];
  [%expect
    {|
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
;;

let%expect_test "widths must be greater than zero" =
  let open Signal in
  require_does_raise [%here] (fun () -> of_bit_string "");
  [%expect
    {|
    ("Width of constant must be greater than zero"
     (width 0)
     (const "")) |}];
  require_does_raise [%here] (fun () -> of_bit_list []);
  [%expect {| ("Width of constant must be greater than zero" (width 0)) |}];
  require_does_raise [%here] (fun () -> of_int ~width:0 0);
  [%expect
    {|
    ("Width of constant must be greater than zero"
     (width 0)
     (const 0)) |}];
  require_does_raise [%here] (fun () -> of_int32 ~width:0 0l);
  [%expect
    {|
    ("Width of constant must be greater than zero"
     (width 0)
     (const 0)) |}];
  require_does_raise [%here] (fun () -> of_int64 ~width:0 0L);
  [%expect
    {|
    ("Width of constant must be greater than zero"
     (width 0)
     (const 0)) |}];
  require_does_raise [%here] (fun () -> of_hex ~width:0 "0");
  [%expect
    {|
    ("Width of constant must be greater than zero"
     (width 0)
     (const 0)) |}];
  require_does_raise [%here] (fun () -> of_hex ~signedness:Signed ~width:0 "0");
  [%expect
    {|
    ("Width of constant must be greater than zero"
     (width 0)
     (const 0)) |}]
;;

let%expect_test "round trip chars" =
  require_does_raise [%here] (fun () -> to_char vdd);
  [%expect {| ("[to_char] signal must be 8 bits wide" (actual_width 1)) |}];
  for i = 0 to 255 do
    let c = Char.of_int_exn i in
    let d = of_char c |> to_char in
    if not (Char.equal c d) then raise_s [%message "Failed to roundtrip char" (c : char)]
  done
;;
