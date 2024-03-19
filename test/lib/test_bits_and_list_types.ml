open! Import
open! Test_bits

let%expect_test "bits against list type" =
  let config bits1 =
    { Config.bits1
    ; bits2 = Bits
    ; prims = Primitive_op.all
    ; iterations = 20
    ; min_bit_width = 1
    ; max_bit_width = 20
    }
  in
  Test.test [%here] (config Bits_list);
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
      (min_bit_width 1)
      (max_bit_width 20)))
    |}];
  Test.test [%here] (config Bool_list);
  [%expect
    {|
    (config (
      (bits1 (
        (name       Bool_list)
        (short_name boollist)
        (module_    <opaque>)))
      (bits2 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    20)
      (min_bit_width 1)
      (max_bit_width 20)))
    |}];
  Test.test [%here] (config X_list);
  [%expect
    {|
    (config (
      (bits1 (
        (name       X_list)
        (short_name xlist)
        (module_    <opaque>)))
      (bits2 (
        (name       Bits)
        (short_name bits)
        (module_    <opaque>)))
      (prims (Add Sub Mulu Muls And Or Xor Not Eq Lt Sel Mux Cat))
      (iterations    20)
      (min_bit_width 1)
      (max_bit_width 20)))
    |}]
;;
