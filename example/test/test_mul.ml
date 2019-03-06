open! Import
open! Mul
open! Bits

let%expect_test "test bits" =
  for bits_x = 0 to 3 do
    for bits_y = 0 to 3 do
      for x=1 to (1 lsl bits_x) - 1 do
        for y=1 to (1 lsl bits_y) - 1 do
          let expected = x * y in
          let mul config =
            Mul.create ~config (module Bits)
              (consti ~width:bits_x x) (consti ~width:bits_y y) in
          let wallace = mul Wallace in
          let dadda   = mul Dadda   in
          if expected <> to_int wallace || expected <> to_int dadda
          then print_s [%message "Tree adder mismatch"
                                   (bits_x : int) (bits_y : int)
                                   (x : int) (y : int)
                                   (expected : int)
                                   (wallace : Bits.t) (dadda : Bits.t)]
        done
      done
    done
  done;
  [%expect {| |}]

let%expect_test "shift" =
  for shift = 0 to 10 do
    let times = (1 lsl shift) in
    let mul config = Mul.create ~config (module Bits) vdd (consti ~width:(num_bits times) times) in
    let wallace = mul Wallace in
    let dadda   = mul Dadda   in
    require_equal [%here] (module Bits) wallace dadda;
    print_s [%message "" (shift : int) (wallace : Bits.t) (dadda : Bits.t)]
  done;
  [%expect {|
    ((shift   0)
     (wallace 01)
     (dadda   01))
    ((shift   1)
     (wallace 010)
     (dadda   010))
    ((shift   2)
     (wallace 0100)
     (dadda   0100))
    ((shift   3)
     (wallace 01000)
     (dadda   01000))
    ((shift   4)
     (wallace 010000)
     (dadda   010000))
    ((shift   5)
     (wallace 0100000)
     (dadda   0100000))
    ((shift   6)
     (wallace 01000000)
     (dadda   01000000))
    ((shift   7)
     (wallace 010000000)
     (dadda   010000000))
    ((shift   8)
     (wallace 0100000000)
     (dadda   0100000000))
    ((shift   9)
     (wallace 01000000000)
     (dadda   01000000000))
    ((shift   10)
     (wallace 010000000000)
     (dadda   010000000000)) |}]

let%expect_test "max" =
  for x = 1 to 3 do
    for y = 1 to 3 do
      let mul config = Mul.create ~config (module Bits)
                         (consti ~width:x (-1)) (consti ~width:y (-1)) in
      let wallace = mul Wallace in
      let dadda   = mul Dadda   in
      require_equal [%here] (module Bits) wallace dadda;
      print_s [%message "" (x : int) (y : int) (wallace : Bits.t) (dadda : Bits.t)]
    done
  done;
  [%expect {|
    ((x       1)
     (y       1)
     (wallace 01)
     (dadda   01))
    ((x       1)
     (y       2)
     (wallace 011)
     (dadda   011))
    ((x       1)
     (y       3)
     (wallace 0111)
     (dadda   0111))
    ((x       2)
     (y       1)
     (wallace 011)
     (dadda   011))
    ((x       2)
     (y       2)
     (wallace 1001)
     (dadda   1001))
    ((x       2)
     (y       3)
     (wallace 10101)
     (dadda   10101))
    ((x       3)
     (y       1)
     (wallace 0111)
     (dadda   0111))
    ((x       3)
     (y       2)
     (wallace 10101)
     (dadda   10101))
    ((x       3)
     (y       3)
     (wallace 110001)
     (dadda   110001)) |}]

let%expect_test "utilization" =
  let utilization config width_a width_b =
    [ Signal.output "v" (Mul.create ~config (module Signal)
                           (Signal.input "a" width_a)
                           (Signal.input "b" width_b)) ]
    |> Circuit.create_exn ~name:"mul"
    |> Circuit_utilization.create
  in
  print_s[%message (utilization Wallace 16 24 : Circuit_utilization.t)];
  [%expect {|
    ("utilization Wallace 16 24" (
      (name mul)
      (adders (
        (count             1)
        (total_bits        40)
        (max_instance_bits 40)))
      (and_gates     ((count 1403) (total_bits 1403)))
      (or_gates      ((count 616)  (total_bits 616)))
      (xor_gates     ((count 711)  (total_bits 711)))
      (constants     ((count 3)    (total_bits 14)))
      (wires         ((count 3)    (total_bits 80)))
      (concatenation ((count 2)    (total_bits 90)))
      (part_selects  ((count 770)  (total_bits 848))))) |}];
  print_s[%message (utilization Dadda 16 24 : Circuit_utilization.t)];
  [%expect {|
    ("utilization Dadda 16 24" (
      (name mul)
      (adders (
        (count             1)
        (total_bits        40)
        (max_instance_bits 40)))
      (and_gates     ((count 1346) (total_bits 1346)))
      (or_gates      ((count 628)  (total_bits 628)))
      (xor_gates     ((count 648)  (total_bits 648)))
      (constants     ((count 5)    (total_bits 16)))
      (wires         ((count 3)    (total_bits 80)))
      (concatenation ((count 2)    (total_bits 90)))
      (part_selects  ((count 770)  (total_bits 848))))) |}]
;;
