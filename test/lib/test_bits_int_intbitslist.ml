open! Import
open! Test_bits

let%expect_test "bitlist<->bits" =
  let config =
    { Config.bits1 = Bits_list
    ; bits2 = Bits
    ; prims = Primitive_op.all
    ; iterations = 4
    ; min_bit_width = 100
    ; max_bit_width = 200
    }
  in
  Test.test [%here] config;
  [%expect
    {|
    (config (
      (bits1 (
        (name       Int_list)
        (short_name intlist)
        (module_    <opaque>)))
      (bits2 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    4)
      (min_bit_width 100)
      (max_bit_width 200)))
    |}];
  Test.test
    [%here]
    { config with iterations = 20; min_bit_width = 30; max_bit_width = 100 };
  [%expect
    {|
    (config (
      (bits1 (
        (name       Int_list)
        (short_name intlist)
        (module_    <opaque>)))
      (bits2 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    20)
      (min_bit_width 30)
      (max_bit_width 100)))
    |}];
  Test.test
    [%here]
    { config with iterations = 200; min_bit_width = 1; max_bit_width = 30 };
  [%expect
    {|
    (config (
      (bits1 (
        (name       Int_list)
        (short_name intlist)
        (module_    <opaque>)))
      (bits2 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    200)
      (min_bit_width 1)
      (max_bit_width 30)))
    |}]
;;
