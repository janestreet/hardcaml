open! Base
open Expect_test_helpers_base
open Hardcaml.Bits

module Make (X : sig
    module Int : Int.S

    val to_unsigned_int : t -> Int.t
    val to_signed_int : t -> Int.t
    val of_unsigned_int : width:int -> Int.t -> t
    val of_signed_int : width:int -> Int.t -> t
    val bigint_of_int : Int.t -> Bigint.t
    val of_base_int : int -> Int.t
  end) =
struct
  open X

  let%expect_test "to_unsigned_int" =
    (* small values *)
    for width = 1 to 3 do
      print_s [%message (width : int)];
      for value = 0 to (1 lsl width) - 1 do
        let to_int ~width value =
          try Ok (to_unsigned_int (of_int_trunc ~width value)) with
          | exn -> Or_error.error_s [%message (exn : exn)]
        in
        print_s [%message "" ~_:(to_int ~width value : Int.t Or_error.t)]
      done
    done;
    [%expect
      {|
      (width 1)
      (Ok 0)
      (Ok 1)
      (width 2)
      (Ok 0)
      (Ok 1)
      (Ok 2)
      (Ok 3)
      (width 3)
      (Ok 0)
      (Ok 1)
      (Ok 2)
      (Ok 3)
      (Ok 4)
      (Ok 5)
      (Ok 6)
      (Ok 7)
      |}];
    (* @ max_value *)
    let of_z (offset : Int.t) =
      try
        let z =
          of_bigint ~width:128 Bigint.(bigint_of_int Int.max_value + bigint_of_int offset)
        in
        let i = to_unsigned_int z in
        if Int.( <> ) Int.(max_value + offset) i
        then raise_s [%message "Internal conversion error"];
        Ok ()
      with
      | exn -> Or_error.error_s [%message (exn : exn)]
    in
    print_s [%message (of_z Int.(neg one) : unit Or_error.t)];
    print_s [%message (of_z Int.zero : unit Or_error.t)];
    print_s [%message (of_z Int.one : unit Or_error.t)];
    [%expect
      {|
      ("of_z (let open Int in neg one)" (Ok ()))
      ("of_z Int.zero" (Ok ()))
      ("of_z Int.one" (
        Error (exn "Failed to convert value to unsigned integer type")))
      |}];
    (* large width, small value *)
    print_s [%message (to_signed_int (of_string "128'd3") : Int.t)];
    print_s [%message (to_signed_int (of_string "128'd37736") : Int.t)];
    [%expect
      {|
      ("to_signed_int (of_string \"128'd3\")" 3)
      ("to_signed_int (of_string \"128'd37736\")" 37736)
      |}]
  ;;

  let%expect_test "of_unsigned_int" =
    for width = 1 to 3 do
      print_s [%message (width : int)];
      for value = -1 to 1 lsl width do
        let of_int ~width value =
          try Ok (of_unsigned_int ~width (of_base_int value)) with
          | exn -> Or_error.error_s [%message (exn : exn)]
        in
        print_s [%message (of_int ~width value : t Or_error.t)]
      done
    done;
    [%expect
      {|
      (width 1)
      ("of_int ~width value" (
        Error (exn ("[of_unsigned_int] input value is less than 0" (x -1)))))
      ("of_int ~width value" (Ok 0))
      ("of_int ~width value" (Ok 1))
      ("of_int ~width value" (
        Error (
          exn (
            "[of_unsigned_int] input value is too large for given width"
            (width     1)
            (max_value 1)
            (x         2)))))
      (width 2)
      ("of_int ~width value" (
        Error (exn ("[of_unsigned_int] input value is less than 0" (x -1)))))
      ("of_int ~width value" (Ok 00))
      ("of_int ~width value" (Ok 01))
      ("of_int ~width value" (Ok 10))
      ("of_int ~width value" (Ok 11))
      ("of_int ~width value" (
        Error (
          exn (
            "[of_unsigned_int] input value is too large for given width"
            (width     2)
            (max_value 3)
            (x         4)))))
      (width 3)
      ("of_int ~width value" (
        Error (exn ("[of_unsigned_int] input value is less than 0" (x -1)))))
      ("of_int ~width value" (Ok 000))
      ("of_int ~width value" (Ok 001))
      ("of_int ~width value" (Ok 010))
      ("of_int ~width value" (Ok 011))
      ("of_int ~width value" (Ok 100))
      ("of_int ~width value" (Ok 101))
      ("of_int ~width value" (Ok 110))
      ("of_int ~width value" (Ok 111))
      ("of_int ~width value" (
        Error (
          exn (
            "[of_unsigned_int] input value is too large for given width"
            (width     3)
            (max_value 7)
            (x         8)))))
      |}];
    (* below and across msb for all int types *)
    List.iter [ 31; 32; 33; 62; 63; 64; 65 ] ~f:(fun width ->
      print_s [%message (width : int)];
      print_s [%message (of_unsigned_int ~width (of_base_int 0) : t)];
      print_s [%message (of_unsigned_int ~width (of_base_int 1) : t)];
      print_s [%message (of_unsigned_int ~width (of_base_int 123456) : t)]);
    [%expect
      {|
      (width 31)
      ("of_unsigned_int ~width (of_base_int 0)" 0000000000000000000000000000000)
      ("of_unsigned_int ~width (of_base_int 1)" 0000000000000000000000000000001)
      ("of_unsigned_int ~width (of_base_int 123456)"
       0000000000000011110001001000000)
      (width 32)
      ("of_unsigned_int ~width (of_base_int 0)" 00000000000000000000000000000000)
      ("of_unsigned_int ~width (of_base_int 1)" 00000000000000000000000000000001)
      ("of_unsigned_int ~width (of_base_int 123456)"
       00000000000000011110001001000000)
      (width 33)
      ("of_unsigned_int ~width (of_base_int 0)" 000000000000000000000000000000000)
      ("of_unsigned_int ~width (of_base_int 1)" 000000000000000000000000000000001)
      ("of_unsigned_int ~width (of_base_int 123456)"
       000000000000000011110001001000000)
      (width 62)
      ("of_unsigned_int ~width (of_base_int 0)"
       00000000000000000000000000000000000000000000000000000000000000)
      ("of_unsigned_int ~width (of_base_int 1)"
       00000000000000000000000000000000000000000000000000000000000001)
      ("of_unsigned_int ~width (of_base_int 123456)"
       00000000000000000000000000000000000000000000011110001001000000)
      (width 63)
      ("of_unsigned_int ~width (of_base_int 0)"
       000000000000000000000000000000000000000000000000000000000000000)
      ("of_unsigned_int ~width (of_base_int 1)"
       000000000000000000000000000000000000000000000000000000000000001)
      ("of_unsigned_int ~width (of_base_int 123456)"
       000000000000000000000000000000000000000000000011110001001000000)
      (width 64)
      ("of_unsigned_int ~width (of_base_int 0)"
       0000000000000000000000000000000000000000000000000000000000000000)
      ("of_unsigned_int ~width (of_base_int 1)"
       0000000000000000000000000000000000000000000000000000000000000001)
      ("of_unsigned_int ~width (of_base_int 123456)"
       0000000000000000000000000000000000000000000000011110001001000000)
      (width 65)
      ("of_unsigned_int ~width (of_base_int 0)"
       00000000000000000000000000000000000000000000000000000000000000000)
      ("of_unsigned_int ~width (of_base_int 1)"
       00000000000000000000000000000000000000000000000000000000000000001)
      ("of_unsigned_int ~width (of_base_int 123456)"
       00000000000000000000000000000000000000000000000011110001001000000)
      |}]
  ;;

  let%expect_test "to_signed_int" =
    (* small values *)
    for width = 1 to 3 do
      print_s [%message (width : int)];
      for value = -(1 lsl (width - 1)) to 1 lsl (width - 1) do
        let to_int ~width value =
          try Ok (to_signed_int (of_int_trunc ~width value)) with
          | exn -> Or_error.error_s [%message (exn : exn)]
        in
        print_s [%message "" ~_:(to_int ~width value : Int.t Or_error.t)]
      done
    done;
    [%expect
      {|
      (width 1)
      (Ok -1)
      (Ok 0)
      (Ok -1)
      (width 2)
      (Ok -2)
      (Ok -1)
      (Ok 0)
      (Ok 1)
      (Ok -2)
      (width 3)
      (Ok -4)
      (Ok -3)
      (Ok -2)
      (Ok -1)
      (Ok 0)
      (Ok 1)
      (Ok 2)
      (Ok 3)
      (Ok -4)
      |}];
    (* @ min and max value *)
    let of_z edge_value (offset : Int.t) =
      try
        let z =
          of_bigint ~width:128 Bigint.(bigint_of_int edge_value + bigint_of_int offset)
        in
        let i = to_signed_int z in
        if Int.( <> ) Int.(edge_value + offset) i
        then raise_s [%message "Internal conversion error"];
        Ok ()
      with
      | exn -> Or_error.error_s [%message (exn : exn)]
    in
    print_s [%message (of_z Int.min_value Int.(neg one) : unit Or_error.t)];
    print_s [%message (of_z Int.min_value Int.zero : unit Or_error.t)];
    print_s [%message (of_z Int.min_value Int.one : unit Or_error.t)];
    print_s [%message (of_z Int.max_value Int.(neg one) : unit Or_error.t)];
    print_s [%message (of_z Int.max_value Int.zero : unit Or_error.t)];
    print_s [%message (of_z Int.max_value Int.one : unit Or_error.t)];
    [%expect
      {|
      ("of_z Int.min_value (let open Int in neg one)"
       (Error (exn "Failed to convert value to signed integer type")))
      ("of_z Int.min_value Int.zero" (Ok ()))
      ("of_z Int.min_value Int.one" (Ok ()))
      ("of_z Int.max_value (let open Int in neg one)" (Ok ()))
      ("of_z Int.max_value Int.zero" (Ok ()))
      ("of_z Int.max_value Int.one" (
        Error (exn "Failed to convert value to signed integer type")))
      |}];
    (* large width, small value *)
    print_s [%message (to_signed_int (of_string "128'd-3") : Int.t)];
    print_s [%message (to_signed_int (of_string "128'd-37736") : Int.t)];
    print_s [%message (to_signed_int (of_string "128'd3") : Int.t)];
    print_s [%message (to_signed_int (of_string "128'd37736") : Int.t)];
    [%expect
      {|
      ("to_signed_int (of_string \"128'd-3\")" -3)
      ("to_signed_int (of_string \"128'd-37736\")" -37736)
      ("to_signed_int (of_string \"128'd3\")" 3)
      ("to_signed_int (of_string \"128'd37736\")" 37736)
      |}]
  ;;

  let%expect_test "of_signed_int" =
    for width = 1 to 3 do
      print_s [%message (width : int)];
      for value = -((1 lsl (width - 1)) + 1) to 1 lsl (width - 1) do
        let of_int ~width value =
          try Ok (of_signed_int ~width (of_base_int value)) with
          | exn -> Or_error.error_s [%message (exn : exn)]
        in
        print_s [%message (of_int ~width value : t Or_error.t)]
      done
    done;
    [%expect
      {|
      (width 1)
      ("of_int ~width value" (
        Error (
          exn (
            "[of_signed_int] input value is too small for given width"
            (width     1)
            (min_value -1)
            (x         -2)))))
      ("of_int ~width value" (Ok 1))
      ("of_int ~width value" (Ok 0))
      ("of_int ~width value" (
        Error (
          exn (
            "[of_signed_int] input value is too large for given width"
            (width     1)
            (max_value 0)
            (x         1)))))
      (width 2)
      ("of_int ~width value" (
        Error (
          exn (
            "[of_signed_int] input value is too small for given width"
            (width     2)
            (min_value -2)
            (x         -3)))))
      ("of_int ~width value" (Ok 10))
      ("of_int ~width value" (Ok 11))
      ("of_int ~width value" (Ok 00))
      ("of_int ~width value" (Ok 01))
      ("of_int ~width value" (
        Error (
          exn (
            "[of_signed_int] input value is too large for given width"
            (width     2)
            (max_value 1)
            (x         2)))))
      (width 3)
      ("of_int ~width value" (
        Error (
          exn (
            "[of_signed_int] input value is too small for given width"
            (width     3)
            (min_value -4)
            (x         -5)))))
      ("of_int ~width value" (Ok 100))
      ("of_int ~width value" (Ok 101))
      ("of_int ~width value" (Ok 110))
      ("of_int ~width value" (Ok 111))
      ("of_int ~width value" (Ok 000))
      ("of_int ~width value" (Ok 001))
      ("of_int ~width value" (Ok 010))
      ("of_int ~width value" (Ok 011))
      ("of_int ~width value" (
        Error (
          exn (
            "[of_signed_int] input value is too large for given width"
            (width     3)
            (max_value 3)
            (x         4)))))
      |}];
    (* below and across msb for all int types *)
    List.iter [ 31; 32; 33; 62; 63; 64; 65 ] ~f:(fun width ->
      print_s [%message (width : int)];
      print_s [%message (of_signed_int ~width (of_base_int (-123456)) : t)];
      print_s [%message (of_signed_int ~width (of_base_int (-1)) : t)];
      print_s [%message (of_signed_int ~width (of_base_int 0) : t)];
      print_s [%message (of_signed_int ~width (of_base_int 1) : t)];
      print_s [%message (of_signed_int ~width (of_base_int 123456) : t)]);
    [%expect
      {|
      (width 31)
      ("of_signed_int ~width (of_base_int (-123456))"
       1111111111111100001110111000000)
      ("of_signed_int ~width (of_base_int (-1))" 1111111111111111111111111111111)
      ("of_signed_int ~width (of_base_int 0)" 0000000000000000000000000000000)
      ("of_signed_int ~width (of_base_int 1)" 0000000000000000000000000000001)
      ("of_signed_int ~width (of_base_int 123456)" 0000000000000011110001001000000)
      (width 32)
      ("of_signed_int ~width (of_base_int (-123456))"
       11111111111111100001110111000000)
      ("of_signed_int ~width (of_base_int (-1))" 11111111111111111111111111111111)
      ("of_signed_int ~width (of_base_int 0)" 00000000000000000000000000000000)
      ("of_signed_int ~width (of_base_int 1)" 00000000000000000000000000000001)
      ("of_signed_int ~width (of_base_int 123456)" 00000000000000011110001001000000)
      (width 33)
      ("of_signed_int ~width (of_base_int (-123456))"
       111111111111111100001110111000000)
      ("of_signed_int ~width (of_base_int (-1))" 111111111111111111111111111111111)
      ("of_signed_int ~width (of_base_int 0)" 000000000000000000000000000000000)
      ("of_signed_int ~width (of_base_int 1)" 000000000000000000000000000000001)
      ("of_signed_int ~width (of_base_int 123456)"
       000000000000000011110001001000000)
      (width 62)
      ("of_signed_int ~width (of_base_int (-123456))"
       11111111111111111111111111111111111111111111100001110111000000)
      ("of_signed_int ~width (of_base_int (-1))"
       11111111111111111111111111111111111111111111111111111111111111)
      ("of_signed_int ~width (of_base_int 0)"
       00000000000000000000000000000000000000000000000000000000000000)
      ("of_signed_int ~width (of_base_int 1)"
       00000000000000000000000000000000000000000000000000000000000001)
      ("of_signed_int ~width (of_base_int 123456)"
       00000000000000000000000000000000000000000000011110001001000000)
      (width 63)
      ("of_signed_int ~width (of_base_int (-123456))"
       111111111111111111111111111111111111111111111100001110111000000)
      ("of_signed_int ~width (of_base_int (-1))"
       111111111111111111111111111111111111111111111111111111111111111)
      ("of_signed_int ~width (of_base_int 0)"
       000000000000000000000000000000000000000000000000000000000000000)
      ("of_signed_int ~width (of_base_int 1)"
       000000000000000000000000000000000000000000000000000000000000001)
      ("of_signed_int ~width (of_base_int 123456)"
       000000000000000000000000000000000000000000000011110001001000000)
      (width 64)
      ("of_signed_int ~width (of_base_int (-123456))"
       1111111111111111111111111111111111111111111111100001110111000000)
      ("of_signed_int ~width (of_base_int (-1))"
       1111111111111111111111111111111111111111111111111111111111111111)
      ("of_signed_int ~width (of_base_int 0)"
       0000000000000000000000000000000000000000000000000000000000000000)
      ("of_signed_int ~width (of_base_int 1)"
       0000000000000000000000000000000000000000000000000000000000000001)
      ("of_signed_int ~width (of_base_int 123456)"
       0000000000000000000000000000000000000000000000011110001001000000)
      (width 65)
      ("of_signed_int ~width (of_base_int (-123456))"
       11111111111111111111111111111111111111111111111100001110111000000)
      ("of_signed_int ~width (of_base_int (-1))"
       11111111111111111111111111111111111111111111111111111111111111111)
      ("of_signed_int ~width (of_base_int 0)"
       00000000000000000000000000000000000000000000000000000000000000000)
      ("of_signed_int ~width (of_base_int 1)"
       00000000000000000000000000000000000000000000000000000000000000001)
      ("of_signed_int ~width (of_base_int 123456)"
       00000000000000000000000000000000000000000000000011110001001000000)
      |}]
  ;;
end

module%test Int = Make (struct
    module Int = Int

    let to_unsigned_int = to_unsigned_int
    let to_signed_int = to_signed_int
    let of_unsigned_int = of_unsigned_int
    let of_signed_int = of_signed_int
    let bigint_of_int = Bigint.of_int
    let of_base_int = Int.of_int_exn
  end)

module%test Int32 = Make (struct
    module Int = Int32

    let to_unsigned_int = to_unsigned_int32
    let to_signed_int = to_signed_int32
    let of_unsigned_int = of_unsigned_int32
    let of_signed_int = of_signed_int32
    let bigint_of_int = Bigint.of_int32
    let of_base_int = Int32.of_int_exn
  end)

module%test Int64 = Make (struct
    module Int = Int64

    let to_unsigned_int = to_unsigned_int64
    let to_signed_int = to_signed_int64
    let of_unsigned_int = of_unsigned_int64
    let of_signed_int = of_signed_int64
    let bigint_of_int = Bigint.of_int64
    let of_base_int = Int64.of_int_exn
  end)
