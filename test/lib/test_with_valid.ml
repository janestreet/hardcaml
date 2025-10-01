open Core
open Import
open Hardcaml_waveterm_cyclesim

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
  { I.a = vdd; b = gnd; c = of_int_trunc ~width:2 1; d = gnd }
;;

let%expect_test "fields value" =
  let open Bits in
  let module Fields = With_valid.Fields.Make (I) in
  let print v =
    print_s
      [%message
        "" ~value:(Fields.value_with_default (module Bits) ~default v : Bits.t I.t)]
  in
  let f =
    { I.a = { With_valid.valid = vdd; value = gnd }
    ; b = { valid = gnd; value = vdd }
    ; c = { valid = vdd; value = of_int_trunc ~width:2 3 }
    ; d = { valid = gnd; value = vdd }
    }
  in
  print f;
  let f =
    { I.a = { With_valid.valid = gnd; value = gnd }
    ; b = { valid = vdd; value = vdd }
    ; c = { valid = gnd; value = of_int_trunc ~width:2 3 }
    ; d = { valid = vdd; value = vdd }
    }
  in
  print f;
  [%expect
    {|
    (value (
      (a 0)
      (b 0)
      (c 11)
      (d 0)))
    (value (
      (a 1)
      (b 1)
      (c 01)
      (d 1)))
    |}]
;;

let%expect_test "wrap value" =
  let open Bits in
  let module Wrap = With_valid.Wrap.Make (I) in
  let print v =
    print_s
      [%message "" ~value:(Wrap.value_with_default (module Bits) ~default v : Bits.t I.t)]
  in
  let f =
    { With_valid.valid = vdd
    ; value = { I.a = gnd; b = vdd; c = of_int_trunc ~width:2 3; d = vdd }
    }
  in
  print f;
  let f = { f with valid = gnd } in
  print f;
  [%expect
    {|
    (value (
      (a 0)
      (b 1)
      (c 11)
      (d 1)))
    (value (
      (a 1)
      (b 0)
      (c 01)
      (d 0)))
    |}]
;;

let%expect_test "with valid Of_signal and Of_always functions and naming" =
  let value_width = 5 in
  let out_valid, out_value =
    let open Signal in
    let in_valid = input "in_valid" 1 in
    let in_value = input "in_value" value_width in
    let scope = Scope.create ~name:"dut" () in
    let spec = Reg_spec.create () ~clock ~clear in
    let%hw.With_valid.Of_signal in_with_valid = With_valid.Of_signal.wires value_width in
    in_with_valid.valid <-- in_valid;
    in_with_valid.value <-- in_value;
    let%hw.With_valid.Of_always with_valid_wire =
      With_valid.Of_always.wire zero ~width:value_width
    in
    let%hw.With_valid.Of_always with_valid_reg =
      With_valid.Of_always.reg spec ~width:value_width
    in
    let open Always in
    compile
      [ with_valid_wire.valid <-- in_with_valid.valid
      ; with_valid_wire.value <-- in_with_valid.value
      ; when_
          with_valid_wire.valid.value
          [ with_valid_reg.valid <-- vdd
          ; with_valid_reg.value <-- with_valid_wire.value.value
          ]
      ];
    let out_valid =
      Signal.output "out_valid" (in_with_valid.valid |: with_valid_reg.valid.value)
    in
    let out_value =
      output
        "out_value"
        (mux2 in_with_valid.valid in_with_valid.value with_valid_reg.value.value)
    in
    out_valid, out_value
  in
  let circuit =
    Circuit.create_exn ~name:"cut_through_latch_with_valid" [ out_valid; out_value ]
  in
  let sim = Cyclesim.create ~config:Cyclesim.Config.trace_all circuit in
  let waves, sim = Waveform.create sim in
  let () =
    let open Bits in
    let clear = Cyclesim.in_port sim "clear" in
    let in_valid = Cyclesim.in_port sim "in_valid" in
    let in_value = Cyclesim.in_port sim "in_value" in
    clear := vdd;
    Cyclesim.cycle sim;
    clear := gnd;
    Cyclesim.cycle ~n:2 sim;
    in_value := of_int_trunc ~width:value_width 0x1;
    Cyclesim.cycle sim;
    in_valid := vdd;
    Cyclesim.cycle sim;
    in_value := of_int_trunc ~width:value_width 0x2;
    Cyclesim.cycle sim;
    in_valid := gnd;
    in_value := of_int_trunc ~width:value_width 0x3;
    Cyclesim.cycle ~n:2 sim
  in
  Waveform.print ~wave_width:1 ~signals_width:30 waves;
  [%expect
    {|
    ┌Signals─────────────────────┐┌Waves─────────────────────────────────┐
    │clock                       ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear                       ││────┐                                 │
    │                            ││    └───────────────────────────      │
    │in_valid                    ││                ┌───────┐             │
    │                            ││────────────────┘       └───────      │
    │                            ││────────────┬───────┬───┬───────      │
    │in_value                    ││ 00         │01     │02 │03           │
    │                            ││────────────┴───────┴───┴───────      │
    │out_valid                   ││                ┌───────────────      │
    │                            ││────────────────┘                     │
    │                            ││────────────────┬───┬───────────      │
    │out_value                   ││ 00             │01 │02               │
    │                            ││────────────────┴───┴───────────      │
    │dut$in_with_valid$valid     ││                ┌───────┐             │
    │                            ││────────────────┘       └───────      │
    │                            ││────────────┬───────┬───┬───────      │
    │dut$in_with_valid$value     ││ 00         │01     │02 │03           │
    │                            ││────────────┴───────┴───┴───────      │
    │dut$with_valid_reg$valid    ││                    ┌───────────      │
    │                            ││────────────────────┘                 │
    │                            ││────────────────────┬───┬───────      │
    │dut$with_valid_reg$value    ││ 00                 │01 │02           │
    │                            ││────────────────────┴───┴───────      │
    │dut$with_valid_wire$valid   ││                ┌───────┐             │
    │                            ││────────────────┘       └───────      │
    │                            ││────────────┬───────┬───┬───────      │
    │dut$with_valid_wire$value   ││ 00         │01     │02 │03           │
    │                            ││────────────┴───────┴───┴───────      │
    │vdd                         ││────────────────────────────────      │
    │                            ││                                      │
    └────────────────────────────┘└──────────────────────────────────────┘
    |}]
;;

let print_wv (x : Bits.t With_valid.t) =
  print_endline [%string "valid = %{x.valid#Bits} ; value = %{x.value#Bits.Unsigned_int}"]
;;

let%expect_test "map_value, output is valid only if input is valid" =
  let open Bits in
  let f (x : Bits.t) : Bits.t = x +:. 15 in
  { With_valid.valid = vdd; value = of_int_trunc ~width:16 100 }
  |> With_valid.map_value ~f
  |> print_wv;
  [%expect {| valid = 1 ; value = 16'd115 |}];
  { With_valid.valid = gnd; value = of_int_trunc ~width:16 100 }
  |> With_valid.map_value ~f
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd115 |}]
;;

let%expect_test "map_value2, output is valid only if both inputs valid" =
  let open Bits in
  let f (x : Bits.t) (y : Bits.t) : Bits.t = x +: y in
  With_valid.map_value2
    (module Bits)
    ~f
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 100 }
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 20 }
  |> print_wv;
  [%expect {| valid = 1 ; value = 16'd120 |}];
  With_valid.map_value2
    (module Bits)
    ~f
    { With_valid.valid = gnd; value = of_int_trunc ~width:16 100 }
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 20 }
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd120 |}];
  With_valid.map_value2
    (module Bits)
    ~f
    { With_valid.valid = gnd; value = of_int_trunc ~width:16 100 }
    { With_valid.valid = gnd; value = of_int_trunc ~width:16 20 }
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd120 |}]
;;

let%expect_test "and_then, output is valid only if input is valid and result is valid" =
  let open Bits in
  let f (x : Bits.t) : Bits.t With_valid.t =
    let sum = x +:. 15 in
    { With_valid.value = sum; valid = sum <:. 100 }
  in
  { With_valid.valid = vdd; value = of_int_trunc ~width:16 50 }
  |> With_valid.and_then (module Bits) ~f
  |> print_wv;
  [%expect {| valid = 1 ; value = 16'd65 |}];
  { With_valid.valid = vdd; value = of_int_trunc ~width:16 90 }
  |> With_valid.and_then (module Bits) ~f
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd105 |}];
  { With_valid.valid = gnd; value = of_int_trunc ~width:16 50 }
  |> With_valid.and_then (module Bits) ~f
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd65 |}]
;;

let%expect_test "and_then2, output is valid only if both inputs valid and result is valid"
  =
  let open Bits in
  let f (x : Bits.t) (y : Bits.t) : Bits.t With_valid.t =
    let sum = x +: y in
    { With_valid.value = sum; valid = sum <:. 100 }
  in
  With_valid.and_then2
    (module Bits)
    ~f
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 30 }
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 40 }
  |> print_wv;
  [%expect {| valid = 1 ; value = 16'd70 |}];
  With_valid.and_then2
    (module Bits)
    ~f
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 70 }
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 40 }
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd110 |}];
  With_valid.and_then2
    (module Bits)
    ~f
    { With_valid.valid = gnd; value = of_int_trunc ~width:16 30 }
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 40 }
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd70 |}];
  With_valid.and_then2
    (module Bits)
    ~f
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 30 }
    { With_valid.valid = gnd; value = of_int_trunc ~width:16 40 }
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd70 |}];
  With_valid.and_then2
    (module Bits)
    ~f
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 70 }
    { With_valid.valid = gnd; value = of_int_trunc ~width:16 40 }
  |> print_wv;
  [%expect {| valid = 0 ; value = 16'd110 |}]
;;

let%expect_test "to_option" =
  let open Bits in
  let x =
    { With_valid.valid = vdd; value = of_int_trunc ~width:16 123 } |> With_valid.to_option
  in
  print_s [%message "" (x : Bits.Unsigned_int.t option)];
  let y =
    { With_valid.valid = gnd; value = of_int_trunc ~width:16 123 } |> With_valid.to_option
  in
  print_s [%message "" (y : Bits.Unsigned_int.t option)];
  [%expect
    {|
    (x (16'd123))
    (y ())
    |}]
;;

let%expect_test "With_valid Of_signal wires raises for different value widths" =
  let from = With_valid.Of_signal.wires 5 in
  (* Correct usage. *)
  ignore (With_valid.Of_signal.wires 5 ~from : Signal.t With_valid.t);
  Expect_test_helpers_base.require_does_raise (fun () ->
    (* Incorrect usage. *)
    ignore (With_valid.Of_signal.wires 6 ~from : Signal.t With_valid.t);
    ());
  [%expect
    {|
    ("The [value] width in [from] does not match the provided [width]."
     (from ((
       (valid (wire (width 1)))
       (value (wire (width 5))))))
     (from_value_width 5)
     (width            6))
    |}]
;;

let%expect_test "Lifting and lowering between With_valid.Fields and With_valid.Wrap for \
                 a Scalar"
  =
  (* When working with a Scalar.S, the type ['a X.t] of the scalar is equivalent to ['a],
     however this is not directly exposed, causing the [With_valid.Fields] version and the
     [With_valid.Wrap] version to differ even though they are functionally equivalent.
     This issue (and the lift/lower functions used to work around this) are documented in
     this test. *)
  let module My_scalar : Types.Scalar = (val Types.scalar ~name:"my_scalar" 8) in
  let module My_scalar_with_valid_fields = With_valid.Fields.Make (My_scalar) in
  let module My_scalar_with_valid_wrap = With_valid.Wrap.Make (My_scalar) in
  let x : Bits.t My_scalar.t = My_scalar.Of_bits.of_unsigned_int 7 in
  let with_valid_fields : Bits.t My_scalar_with_valid_fields.t =
    My_scalar.map x ~f:(fun value -> { Hardcaml.With_valid.value; valid = Bits.vdd })
  in
  print_s
    [%message
      ""
        (with_valid_fields : Bits.t My_scalar_with_valid_fields.t)
        (My_scalar.lift_with_valid with_valid_fields : Bits.t My_scalar_with_valid_wrap.t)];
  [%expect
    {|
    ((with_valid_fields (
       my_scalar (
         (valid 1)
         (value 00000111))))
     ("My_scalar.lift_with_valid with_valid_fields"
      ((valid 1) (value (my_scalar 00000111)))))
    |}];
  let with_valid_wrap : Bits.t My_scalar_with_valid_wrap.t =
    { value = x; valid = Bits.vdd }
  in
  print_s
    [%message
      ""
        (with_valid_wrap : Bits.t My_scalar_with_valid_wrap.t)
        (My_scalar.lower_with_valid with_valid_wrap
         : Bits.t My_scalar_with_valid_fields.t)];
  [%expect
    {|
    ((with_valid_wrap ((valid 1) (value (my_scalar 00000111))))
     ("My_scalar.lower_with_valid with_valid_wrap"
      (my_scalar (
        (valid 1)
        (value 00000111)))))
    |}]
;;
