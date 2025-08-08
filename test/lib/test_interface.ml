open! Import

module I = struct
  type 'a t =
    { x : 'a [@bits 4]
    ; y : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

let%expect_test "[equal]" =
  let equal t1 t2 = print_s [%sexp (I.equal Int.equal t1 t2 : bool)] in
  let t1 = { I.x = 13; y = 13 } in
  let t2 = { I.x = 13; y = 14 } in
  equal t1 t1;
  [%expect {| true |}];
  equal t1 t2;
  [%expect {| false |}]
;;

(* Functors are applying correctly wrt [Of_bits] and [Of_signal]. *)
let%expect_test "bits and signals" =
  let test (module C : I.Comb) = print_s [%sexp (C.of_unsigned_int 10 : C.t)] in
  test (module I.Of_bits);
  [%expect
    {|
    ((x 1010)
     (y 00001010))
    |}];
  test (module I.Of_signal);
  [%expect
    {|
    ((x (const (width 4) (value 0b1010)))
     (y (const (width 8) (value 0b00001010))))
    |}]
;;

let%expect_test "[port_names], [port_widths], [widths]" =
  print_s [%sexp (I.port_names : string I.t)];
  [%expect
    {|
    ((x x)
     (y y))
    |}];
  print_s [%sexp (I.port_widths : int I.t)];
  [%expect
    {|
    ((x 4)
     (y 8))
    |}];
  print_s [%sexp (I.Of_bits.widths { I.x = Bits.vdd; y = Bits.empty } : int I.t)];
  [%expect
    {|
    ((x 1)
     (y 0))
    |}]
;;

let a : int I.t = { x = 1; y = 5 }
let b : float I.t = { x = 2.; y = 6. }
let c : string I.t = { x = "3"; y = "7" }
let d : char I.t = { x = '4'; y = '8' }
let e : bool I.t = { x = false; y = true }

let%expect_test "zips" =
  print_s [%sexp (I.zip a b : (int * float) I.t)];
  [%expect
    {|
    ((x (1 2))
     (y (5 6)))
    |}];
  print_s [%sexp (I.zip3 a b c : (int * float * string) I.t)];
  [%expect
    {|
    ((x (1 2 3))
     (y (5 6 7)))
    |}];
  print_s [%sexp (I.zip4 a b c d : (int * float * string * char) I.t)];
  [%expect
    {|
    ((x (1 2 3 4))
     (y (5 6 7 8)))
    |}];
  print_s [%sexp (I.zip5 a b c d e : (int * float * string * char * bool) I.t)];
  [%expect
    {|
    ((x (1 2 3 4 false))
     (y (5 6 7 8 true)))
    |}]
;;

let%expect_test "maps" =
  let f a b = a, b in
  print_s [%sexp (I.map2 a b ~f : (int * float) I.t)];
  [%expect
    {|
    ((x (1 2))
     (y (5 6)))
    |}];
  let f a b c = a, b, c in
  print_s [%sexp (I.map3 a b c ~f : (int * float * string) I.t)];
  [%expect
    {|
    ((x (1 2 3))
     (y (5 6 7)))
    |}];
  let f a b c d = a, b, c, d in
  print_s [%sexp (I.map4 a b c d ~f : (int * float * string * char) I.t)];
  [%expect
    {|
    ((x (1 2 3 4))
     (y (5 6 7 8)))
    |}];
  let f a b c d e = a, b, c, d, e in
  print_s [%sexp (I.map5 a b c d e ~f : (int * float * string * char * bool) I.t)];
  [%expect
    {|
    ((x (1 2 3 4 false))
     (y (5 6 7 8 true)))
    |}]
;;

let%expect_test "iters" =
  I.iter3 I.port_names_and_widths a b ~f:(fun (name, bits) a b ->
    printf "%s[%i]: %i %f\n" name bits a b);
  [%expect
    {|
    x[4]: 1 2.000000
    y[8]: 5 6.000000
    |}];
  I.iter4 I.port_names_and_widths a b c ~f:(fun (name, bits) a b c ->
    printf "%s[%i]: %i %f %s\n" name bits a b c);
  [%expect
    {|
    x[4]: 1 2.000000 3
    y[8]: 5 6.000000 7
    |}];
  I.iter5 I.port_names_and_widths a b c d ~f:(fun (name, bits) a b c d ->
    printf "%s[%i]: %i %f %s %c\n" name bits a b c d);
  [%expect
    {|
    x[4]: 1 2.000000 3 4
    y[8]: 5 6.000000 7 8
    |}]
;;

let%expect_test "offsets" =
  print_s [%sexp (I.offsets () : int I.t)];
  [%expect
    {|
    ((x 0)
     (y 4))
    |}];
  print_s [%sexp (I.offsets ~rev:true () : int I.t)];
  [%expect
    {|
    ((x 8)
     (y 0))
    |}]
;;

let%expect_test "unsafe assoc list" =
  let alist = I.Unsafe_assoc_by_port_name.to_alist a in
  print_s [%sexp (alist : (string * int) list)];
  [%expect
    {|
    ((x 1)
     (y 5))
    |}];
  print_s [%sexp (I.Unsafe_assoc_by_port_name.of_alist alist : int I.t)];
  [%expect
    {|
    ((x 1)
     (y 5))
    |}];
  require_does_raise (fun () -> I.of_alist []);
  [%expect
    {|
    ("[Interface.Make.of_alist] Field not provided"
     (missing_field_name (x))
     (interface (
       (x 4)
       (y 8))))
    |}]
;;

let%expect_test "safe assoc list" =
  let module I = struct
    type 'a t =
      { x : 'a
      ; y : 'a [@rtlname "x"]
      }
    [@@deriving hardcaml]
  end
  in
  (* show port names are the same *)
  print_s [%message (I.port_names : string I.t)];
  [%expect
    {|
    (I.port_names (
      (x x)
      (y x)))
    |}];
  let alist = I.to_alist { x = 1; y = 2 } in
  let roundtrip = I.of_alist alist in
  print_s [%message (roundtrip : int I.t)];
  [%expect
    {|
    (roundtrip (
      (x 1)
      (y 2)))
    |}]
;;

let%expect_test "[wires]" =
  print_s [%sexp (I.Of_signal.wires () : Signal.t I.t)];
  [%expect
    {|
    ((x (wire (width 4)))
     (y (wire (width 8))))
    |}];
  print_s [%sexp (I.Of_signal.wires ~named:true () : Signal.t I.t)];
  [%expect {| ((x (wire (names (x)) (width 4))) (y (wire (names (y)) (width 8)))) |}];
  let from = { I.x = Signal.vdd; y = Signal.gnd } in
  print_s [%sexp (I.Of_signal.wires ~from () : Signal.t I.t)];
  [%expect
    {|
    ((x (wire (width 1) (data_in 0b1)))
     (y (wire (width 1) (data_in 0b0))))
    |}];
  print_s [%sexp (I.Of_signal.wires ~named:true ~from () : Signal.t I.t)];
  [%expect
    {|
    ((x (
       wire
       (names (x))
       (width   1)
       (data_in 0b1)))
     (y (
       wire
       (names (y))
       (width   1)
       (data_in 0b0))))
    |}]
;;

let%expect_test "[of_int], [of_ints]" =
  print_s [%sexp (I.Of_bits.zero () : Bits.t I.t)];
  [%expect
    {|
    ((x 0000)
     (y 00000000))
    |}];
  print_s [%sexp (I.Of_bits.of_signed_int (-1) : Bits.t I.t)];
  [%expect
    {|
    ((x 1111)
     (y 11111111))
    |}];
  print_s [%sexp (I.Of_bits.of_unsigned_ints { x = 3; y = 6 } : Bits.t I.t)];
  [%expect
    {|
    ((x 0011)
     (y 00000110))
    |}]
;;

let%expect_test "[pack], [unpack]" =
  print_s
    [%sexp
      (I.Of_bits.pack
         (I.map2 I.port_widths { x = -1; y = 0 } ~f:(fun width ->
            Bits.of_int_trunc ~width))
       : Bits.t)];
  [%expect {| 000000001111 |}];
  print_s [%sexp (I.Of_bits.(unpack (Bits.of_bit_string "000000001111")) : Bits.t I.t)];
  [%expect
    {|
    ((x 1111)
     (y 00000000))
    |}];
  print_s
    [%sexp
      (I.Of_bits.(
         pack
           ~rev:true
           (I.map2 I.port_widths { x = -1; y = 0 } ~f:(fun width ->
              Bits.of_int_trunc ~width)))
       : Bits.t)];
  [%expect {| 111100000000 |}];
  print_s
    [%sexp
      (I.Of_bits.(unpack ~rev:true (Bits.of_bit_string "111100000000")) : Bits.t I.t)];
  [%expect
    {|
    ((x 1111)
     (y 00000000))
    |}]
;;

let%expect_test "[pack], [unpack] validate widths" =
  (* Expect pack to fail if a field has an incorrect port width. *)
  require_does_raise (fun () ->
    ignore (I.Of_bits.pack { x = Bits.vdd; y = Bits.zero 2 } : Bits.t));
  [%expect
    {|
    ("Port width mismatch in interface"
      (port_name      x)
      (expected_width 4)
      (actual_width   1))
    |}];
  require_does_raise (fun () ->
    ignore (I.Of_bits.pack { x = Bits.ones 4; y = Bits.zero 32 } : Bits.t));
  [%expect
    {|
    ("Port width mismatch in interface"
      (port_name      y)
      (expected_width 8)
      (actual_width   32))
    |}];
  (* Expect unpack to fail if the signal is not exactly the size of the sum of all port
     widths. *)
  require_does_raise (fun () -> ignore (I.Of_bits.unpack (Bits.zero 11) : Bits.t I.t));
  [%expect
    {|
    ("Unpack called with an incorrect width"
      (expected_width 12)
      (actual_width   11))
    |}];
  require_does_raise (fun () -> ignore (I.Of_bits.unpack (Bits.zero 13) : Bits.t I.t));
  [%expect
    {|
    ("Unpack called with an incorrect width"
      (expected_width 12)
      (actual_width   13))
    |}]
;;

let%expect_test "[of_interface_list], [to_interface_list]" =
  let i1 = { I.x = [ 10; 20; 30 ]; y = [ 100; 200; 300 ] } in
  let i2 = I.to_interface_list i1 in
  print_s [%sexp (i2 : int I.t list)];
  [%expect
    {|
    (((x 10) (y 100))
     ((x 20) (y 200))
     ((x 30) (y 300)))
    |}];
  print_s [%sexp (I.of_interface_list i2 : int list I.t)];
  [%expect
    {|
    ((x (10  20  30))
     (y (100 200 300)))
    |}];
  require_does_raise (fun () -> I.to_interface_list { I.x = [ 1 ]; y = [ 2; 3 ] });
  [%expect
    {|
    ("[Interface.Make.to_interface_list] field list lengths must be the same"
     (lengths (
       (x 1)
       (y 2))))
    |}]
;;

let%expect_test "[mux]" =
  let data = List.init 4 ~f:I.Of_bits.of_unsigned_int in
  let results =
    List.init 4 ~f:(fun sel -> I.Of_bits.mux (Bits.of_int_trunc ~width:2 sel) data)
  in
  print_s [%sexp (results : Bits.t I.t list)];
  [%expect
    {|
    (((x 0000) (y 00000000))
     ((x 0001) (y 00000001))
     ((x 0010) (y 00000010))
     ((x 0011) (y 00000011)))
    |}];
  require_does_raise (fun () -> I.Of_bits.mux Bits.vdd []);
  [%expect {| "[mux] got empty list" |}]
;;

let%expect_test "concat" =
  print_s
    [%sexp
      (I.Of_bits.(concat [ of_unsigned_int 0; of_signed_int (-1); of_unsigned_int 0 ])
       : Bits.t I.t)];
  [%expect
    {|
    ((x 000011110000)
     (y 000000001111111100000000))
    |}];
  require_does_raise (fun () -> I.Of_bits.concat []);
  [%expect {| "[concat] got empty list" |}]
;;

let%expect_test "apply_names" =
  print_s [%sexp (I.Of_signal.zero () |> I.Of_signal.apply_names : Signal.t I.t)];
  [%expect
    {|
    ((x (
       const
       (names (x))
       (width 4)
       (value 0b0000)))
     (y (
       const
       (names (y))
       (width 8)
       (value 0b00000000))))
    |}];
  print_s
    [%sexp (I.Of_signal.zero () |> I.Of_signal.apply_names ~prefix:"_" : Signal.t I.t)];
  [%expect
    {|
    ((x (
       const
       (names (_x))
       (width 4)
       (value 0b0000)))
     (y (
       const
       (names (_y))
       (width 8)
       (value 0b00000000))))
    |}];
  print_s
    [%sexp (I.Of_signal.zero () |> I.Of_signal.apply_names ~suffix:"_" : Signal.t I.t)];
  [%expect
    {|
    ((x (
       const
       (names (x_))
       (width 4)
       (value 0b0000)))
     (y (
       const
       (names (y_))
       (width 8)
       (value 0b00000000))))
    |}];
  let of_always = I.Of_always.wire Signal.zero in
  I.Of_always.apply_names ~prefix:"?" ~suffix:"!" of_always;
  print_s [%sexp (of_always : Always.Variable.t I.t)];
  [%expect {| ((x (wire (names (?x!)) (width 4))) (y (wire (names (?y!)) (width 8)))) |}]
;;

let%expect_test "assert_widths" =
  require_does_not_raise (fun () -> I.Of_signal.zero () |> I.Of_signal.assert_widths);
  [%expect {| |}];
  require_does_raise (fun () ->
    I.Of_signal.zero () |> I.map ~f:Signal.ue |> I.Of_signal.assert_widths);
  [%expect
    {|
    ("Port width mismatch in interface"
      (port_name      x)
      (expected_width 4)
      (actual_width   5))
    |}]
;;

let priority_sel_tests =
  let x = I.Of_bits.of_unsigned_ints { x = 1; y = 2 } in
  let y = I.Of_bits.of_unsigned_ints { x = 3; y = 4 } in
  (* tests for 0, 1, and 2 case selects *)
  [| [| [] |]
   ; [| [ { With_valid.valid = Bits.gnd; value = x } ]
      ; [ { With_valid.valid = Bits.vdd; value = y } ]
     |]
   ; [| [ { With_valid.valid = Bits.gnd; value = x }
        ; { With_valid.valid = Bits.gnd; value = y }
        ]
      ; [ { With_valid.valid = Bits.vdd; value = x }
        ; { With_valid.valid = Bits.gnd; value = y }
        ]
      ; [ { With_valid.valid = Bits.gnd; value = x }
        ; { With_valid.valid = Bits.vdd; value = y }
        ]
      ; [ { With_valid.valid = Bits.vdd; value = x }
        ; { With_valid.valid = Bits.vdd; value = y }
        ]
     |]
  |]
;;

let%expect_test "priority_select" =
  require_does_raise (fun () ->
    ignore
      (I.Of_bits.(priority_select priority_sel_tests.(0).(0))
       : (Bits.t, Bits.t I.t) With_valid.t2));
  [%expect {| "[priority_select] requires at least one input" |}];
  Array.iter priority_sel_tests.(1) ~f:(fun test ->
    print_s
      [%sexp (I.Of_bits.(priority_select test) : (Bits.t, Bits.t I.t) With_valid.t2)]);
  [%expect
    {|
    ((valid 0)
     (value (
       (x 0001)
       (y 00000010))))
    ((valid 1)
     (value (
       (x 0011)
       (y 00000100))))
    |}];
  Array.iter priority_sel_tests.(2) ~f:(fun test ->
    print_s
      [%sexp (I.Of_bits.(priority_select test) : (Bits.t, Bits.t I.t) With_valid.t2)]);
  [%expect
    {|
    ((valid 0)
     (value (
       (x 0011)
       (y 00000100))))
    ((valid 1)
     (value (
       (x 0001)
       (y 00000010))))
    ((valid 1)
     (value (
       (x 0011)
       (y 00000100))))
    ((valid 1)
     (value (
       (x 0001)
       (y 00000010))))
    |}]
;;

let%expect_test "priority_select_with_default" =
  let default = I.Of_bits.of_unsigned_ints { x = 5; y = 6 } in
  require_does_raise (fun () ->
    ignore
      (I.Of_bits.(priority_select_with_default ~default priority_sel_tests.(0).(0))
       : Bits.t I.t));
  [%expect {| "[priority_select_with_default] requires at least one input" |}];
  Array.iter priority_sel_tests.(1) ~f:(fun test ->
    print_s [%sexp (I.Of_bits.(priority_select_with_default ~default test) : Bits.t I.t)]);
  [%expect
    {|
    ((x 0101)
     (y 00000110))
    ((x 0011)
     (y 00000100))
    |}];
  Array.iter priority_sel_tests.(2) ~f:(fun test ->
    print_s [%sexp (I.Of_bits.(priority_select_with_default ~default test) : Bits.t I.t)]);
  [%expect
    {|
    ((x 0101)
     (y 00000110))
    ((x 0001)
     (y 00000010))
    ((x 0011)
     (y 00000100))
    ((x 0001)
     (y 00000010))
    |}]
;;

let%expect_test "onehot_select" =
  require_does_raise (fun () ->
    ignore (I.Of_bits.(onehot_select priority_sel_tests.(0).(0)) : Bits.t I.t));
  [%expect {| "[onehot_select] requires at least one input" |}];
  Array.iter priority_sel_tests.(1) ~f:(fun test ->
    print_s [%sexp (I.Of_bits.(onehot_select test) : Bits.t I.t)]);
  [%expect
    {|
    ((x 0000)
     (y 00000000))
    ((x 0011)
     (y 00000100))
    |}];
  Array.iter priority_sel_tests.(2) ~f:(fun test ->
    print_s [%sexp (I.Of_bits.(onehot_select test) : Bits.t I.t)]);
  [%expect
    {|
    ((x 0000)
     (y 00000000))
    ((x 0001)
     (y 00000010))
    ((x 0011)
     (y 00000100))
    ((x 0011)
     (y 00000110))
    |}]
;;

module Another_module = struct
  type 'a t = { value : 'a [@bits 16] } [@@deriving hardcaml]
end

type 'a t =
  { foo : 'a [@bits 8] [@rtlname "value"]
  ; bar : 'a Another_module.t
  ; baz : 'a Another_module.t
  }
[@@deriving hardcaml ~rtlmangle:false]

let%expect_test "all names are 'value'." =
  print_s [%message (port_names : string t)];
  [%expect
    {|
    (port_names (
      (foo value)
      (bar ((value value)))
      (baz ((value value)))))
    |}]
;;

let%expect_test "pack and unpack work, even though port names are shared." =
  let packed =
    { foo = 0x1; bar = { value = 0xbb }; baz = { value = 0xaa } }
    |> map2 port_widths ~f:(fun width -> Bits.of_int_trunc ~width)
    |> Of_bits.pack
  in
  let unpacked = Of_bits.unpack packed |> map ~f:Bits.to_int_trunc in
  print_s [%message (unpacked : Int.Hex.t t)];
  (* We don't actually print out the (wrong) values, as we now raise in this
       situation. *)
  [%expect
    {|
    (unpacked (
      (foo 0x1)
      (bar ((value 0xbb)))
      (baz ((value 0xaa)))))
    |}]
;;
