open! Import
open! Test_bits

let%expect_test "bits<->raw-int" =
  let config =
    { Config.bits1 = Bits
    ; bits2 = Mutable_Bits_int_array
    ; prims = Primitive_op.all
    ; iterations = 20
    ; min_bit_width = 900
    ; max_bit_width = 1000
    }
  in
  Test.test [%here] config;
  [%expect
    {|
    (config (
      (bits1 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (bits2 (
        (name       Mutable.Bits_int_array)
        (short_name raw-int)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    20)
      (min_bit_width 900)
      (max_bit_width 1000))) |}];
  Test.test
    [%here]
    { config with iterations = 200; min_bit_width = 30; max_bit_width = 200 };
  [%expect
    {|
    (config (
      (bits1 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (bits2 (
        (name       Mutable.Bits_int_array)
        (short_name raw-int)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    200)
      (min_bit_width 30)
      (max_bit_width 200))) |}];
  Test.test
    [%here]
    { config with iterations = 2000; min_bit_width = 1; max_bit_width = 30 };
  [%expect
    {|
    (config (
      (bits1 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (bits2 (
        (name       Mutable.Bits_int_array)
        (short_name raw-int)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    2000)
      (min_bit_width 1)
      (max_bit_width 30))) |}]
;;
