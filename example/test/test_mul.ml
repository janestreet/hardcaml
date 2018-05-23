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
            Mul.create ~config (module Bits) (consti bits_x x) (consti bits_y y) in
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
    let mul config = Mul.create ~config (module Bits) vdd (consti (num_bits times) times) in
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
      let mul config = Mul.create ~config (module Bits) (consti x (-1)) (consti y (-1)) in
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
