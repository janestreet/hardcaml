open Core
open Hardcaml

module I = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    ; c : 'a [@bits 2]
    ; d : 'a
    }
  [@@deriving hardcaml]
end

let default =
  let open Bits in
  { I.a = vdd; b = gnd; c = of_int ~width:2 1; d = gnd }
;;

let%expect_test "fields value" =
  let open Bits in
  let module Fields = With_valid.Fields.Make (I) in
  let print v =
    print_s [%message "" ~value:(Fields.value (module Bits) ~default v : Bits.t I.t)]
  in
  let f =
    { I.a = { With_valid.valid = vdd; value = gnd }
    ; b = { valid = gnd; value = vdd }
    ; c = { valid = vdd; value = of_int ~width:2 3 }
    ; d = { valid = gnd; value = vdd }
    }
  in
  print f;
  let f =
    { I.a = { With_valid.valid = gnd; value = gnd }
    ; b = { valid = vdd; value = vdd }
    ; c = { valid = gnd; value = of_int ~width:2 3 }
    ; d = { valid = vdd; value = vdd }
    }
  in
  print f;
  [%expect
    {|
    (value ((a 0) (b 0) (c 11) (d 0)))
    (value ((a 1) (b 1) (c 01) (d 1)))
    |}]
;;

let%expect_test "wrap value" =
  let open Bits in
  let module Wrap = With_valid.Wrap.Make (I) in
  let print v =
    print_s [%message "" ~value:(Wrap.value (module Bits) ~default v : Bits.t I.t)]
  in
  let f =
    { With_valid.valid = vdd
    ; value = { I.a = gnd; b = vdd; c = of_int ~width:2 3; d = vdd }
    }
  in
  print f;
  let f = { f with valid = gnd } in
  print f;
  [%expect
    {|
    (value ((a 0) (b 1) (c 11) (d 1)))
    (value ((a 1) (b 0) (c 01) (d 0)))
    |}]
;;
