open! Import
open! Prefix_sum

let%expect_test "running sum" =
  let inputs = [ 2; 5; 3; 1 ] in
  List.iter Config.all ~f:(fun config ->
    print_s [%message
      ""
        (config : Config.t)
        ~outpus:(eval ~config ~operator:(+) inputs : int list)]);
  [%expect {|
    ((config Serial) (outpus (2 7 10 11)))
    ((config Sklansky) (outpus (2 7 10 11)))
    ((config Brent_kung) (outpus (2 7 10 11)))
    ((config Kogge_stone) (outpus (2 7 10 11))) |}];
;;

let%expect_test "parallel prefix adder" =
  List.iter Config.all ~f:(fun config ->
    print_s [%sexp (config : Config.t)];
    List.iter
      [  0,  1, 0
      ; 14,  3, 1
      ;  7, 30, 1
      ; 19,  2, 1
      ; 28, 12, 0 ]
      ~f:(fun (input1, input2, carry) ->
        (* Brent_kung and Kogge_stone require power of 2 width arguments. *)
        let nbits = num_bits (max input1 input2) |> Int.ceil_pow2 in
        let output =
          create (module Bits) ~config
            ~input1:(Bits.consti ~width:nbits input1)
            ~input2:(Bits.consti ~width:nbits input2)
            ~carry_in:(if carry = 1 then Bits.vdd else Bits.gnd)
          |> Bits.to_int
        in
        require_equal [%here] (module Int) (input1 + input2 + carry) output;
        print_s [%message
          "" ~_:(input1 : int) "+" ~_:(input2 : int) "+" ~_:(carry : int)
            "=" ~_:(output : int)]));
  [%expect {|
    Serial
    (0 + 1 + 0 = 1)
    (14 + 3 + 1 = 18)
    (7 + 30 + 1 = 38)
    (19 + 2 + 1 = 22)
    (28 + 12 + 0 = 40)
    Sklansky
    (0 + 1 + 0 = 1)
    (14 + 3 + 1 = 18)
    (7 + 30 + 1 = 38)
    (19 + 2 + 1 = 22)
    (28 + 12 + 0 = 40)
    Brent_kung
    (0 + 1 + 0 = 1)
    (14 + 3 + 1 = 18)
    (7 + 30 + 1 = 38)
    (19 + 2 + 1 = 22)
    (28 + 12 + 0 = 40)
    Kogge_stone
    (0 + 1 + 0 = 1)
    (14 + 3 + 1 = 18)
    (7 + 30 + 1 = 38)
    (19 + 2 + 1 = 22)
    (28 + 12 + 0 = 40) |}];
;;

let%expect_test "utilization" =
  let utilization config width =
    [ Signal.output "v" (create ~config (module Signal)
                           ~input1:(Signal.input "a" width)
                           ~input2:(Signal.input "b" width)
                           ~carry_in:Signal.gnd) ]
    |> Circuit.create_exn ~name:"mul"
    |> Circuit_utilization.create
  in
  print_s[%message (utilization Serial 16 : Circuit_utilization.t)];
  [%expect {|
    ("utilization Serial 16" (
      (name mul)
      (and_gates     ((count 62) (total_bits 62)))
      (or_gates      ((count 31) (total_bits 31)))
      (xor_gates     ((count 33) (total_bits 33)))
      (constants     ((count 1)  (total_bits 1)))
      (wires         ((count 3)  (total_bits 49)))
      (concatenation ((count 1)  (total_bits 17)))
      (part_selects  ((count 32) (total_bits 32))))) |}];
  print_s[%message (utilization Sklansky 16 : Circuit_utilization.t)];
  [%expect {|
    ("utilization Sklansky 16" (
      (name mul)
      (and_gates     ((count 96) (total_bits 96)))
      (or_gates      ((count 48) (total_bits 48)))
      (xor_gates     ((count 33) (total_bits 33)))
      (constants     ((count 1)  (total_bits 1)))
      (wires         ((count 3)  (total_bits 49)))
      (concatenation ((count 1)  (total_bits 17)))
      (part_selects  ((count 32) (total_bits 32))))) |}];
  print_s[%message (utilization Brent_kung 16 : Circuit_utilization.t)];
  [%expect {|
    ("utilization Brent_kung 16" (
      (name mul)
      (and_gates     ((count 84) (total_bits 84)))
      (or_gates      ((count 42) (total_bits 42)))
      (xor_gates     ((count 33) (total_bits 33)))
      (constants     ((count 1)  (total_bits 1)))
      (wires         ((count 3)  (total_bits 49)))
      (concatenation ((count 1)  (total_bits 17)))
      (part_selects  ((count 32) (total_bits 32))))) |}];
  print_s[%message (utilization Kogge_stone 16 : Circuit_utilization.t)];
  [%expect {|
    ("utilization Kogge_stone 16" (
      (name mul)
      (and_gates     ((count 130) (total_bits 130)))
      (or_gates      ((count 65)  (total_bits 65)))
      (xor_gates     ((count 33)  (total_bits 33)))
      (constants     ((count 1)   (total_bits 1)))
      (wires         ((count 3)   (total_bits 49)))
      (concatenation ((count 1)   (total_bits 17)))
      (part_selects  ((count 32)  (total_bits 32))))) |}]
